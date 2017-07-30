/*
 *  TP-LINK TL-WR1041 v2 board support
 *
 *  Copyright (C) 2010-2012 Gabor Juhos <juhosg@openwrt.org>
 *  Copyright (C) 2011-2012 Anan Huang <axishero@foxmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License version 2 as published
 *  by the Free Software Foundation.
 */

#include <linux/pci.h>
#include <linux/phy.h>
#include <linux/platform_device.h>
#include <linux/ath9k_platform.h>
#include <linux/ar8216_platform.h>

#include "machtypes.h"
#include <asm/mach-ath79/ar71xx_regs.h>
#include <asm/mach-ath79/ath79.h>
#include <asm/mach-ath79/mach-linino.h>

#include "common.h"
#include "dev-ap9x-pci.h"
#include "dev-eth.h"
#include "dev-gpio-buttons.h"
#include "dev-leds-gpio.h"
#include "dev-m25p80.h"
#include "dev-spi.h"
#include "dev-usb.h"
#include "dev-wmac.h"
#include "machtypes.h"
#include "gpio.h"
#include "linux/gpio.h"
#include <linux/spi/spi_gpio.h>

#define TIAN_GPIO_MCU_RESET	0

#define TIAN_GPIO_SWDIO			13
#define TIAN_GPIO_SWDCLK		14
#define TIAN_GPIO_CONF_BTN	17
#define TIAN_GPIO_LED0		11
#define TIAN_GPIO_LED1		12
#define TIAN_GPIO_LED2		2
#define TIAN_GPIO_GPIO3		3

#define TIAN_GPIO_UART1_RX	9
#define TIAN_GPIO_UART1_TX	10
#define TIAN_GPIO_OE2		15
#define TIAN_GPIO_UART_POL	GPIOF_OUT_INIT_LOW

#define AR934X_GPIO_UART1_TD_OUT	79	/* table 2.16 */
#define AR934X_GPIO_UART0_SOUT	24	/* table 2.16 */

#define TIAN_GPIO_SPI_INTERRUPT	16
#define DS_PCIE_CALDATA_OFFSET	0x5000

/* * * * * * * * * * * * * * * * * * * LED * * * * * * * * * * * * * * * * * */

static struct gpio_led tian_leds_gpio[] __initdata = {
	{
		.name		= "usb",
		.gpio		= TIAN_GPIO_LED0,
		.active_low	= 1,
	}, {
		.name		= "wlan",
		.gpio		= TIAN_GPIO_LED1,
		.active_low	= 1,
	},
	{
		.name		= "LED2",
		.gpio		= TIAN_GPIO_LED2,
		.active_low	= 1,
	}
};

static struct gpio_keys_button tian_gpio_keys[] __initdata = {
	{
		.desc		= "configuration button",
		.type		= EV_KEY,
		.code		= KEY_RESTART,
		.debounce_interval = DS_KEYS_DEBOUNCE_INTERVAL,
		.gpio		= TIAN_GPIO_CONF_BTN,
		.active_low	= 1,
	}
};

static struct ar8327_pad_cfg db120_ar8327_pad0_cfg = {
	.mode = AR8327_PAD_MAC_RGMII,
	.txclk_delay_en = true,
	.rxclk_delay_en = true,
	.txclk_delay_sel = AR8327_CLK_DELAY_SEL1,
	.rxclk_delay_sel = AR8327_CLK_DELAY_SEL2,
};

static struct ar8327_platform_data db120_ar8327_data = {
	.pad0_cfg = &db120_ar8327_pad0_cfg,
	.port0_cfg = {
		.force_link = 1,
		.speed = AR8327_PORT_SPEED_1000,
		.duplex = 1,
		.txpause = 1,
		.rxpause = 1,
	}
};

static struct mdio_board_info db120_mdio0_info[] = {
	{
		.bus_id = "ag71xx-mdio.0",
		.phy_addr = 0,
		.platform_data = &db120_ar8327_data,
	},
};

/**
 * Enable the software SPI controller emulated by GPIO signals
 */
static void ds_register_spi(void) {
	pr_info("mach-linino: enabling GPIO SPI Controller");

	/* Enable level shifter on SPI signals */
	gpio_set_value(TIAN_GPIO_OE2, 0);
}

/*
 * Enable level shifters
 */
static void __init ds_setup_level_shifter_oe(void)
{
	int err;

	/* enable OE2 of level shifter */
    pr_info("Setting GPIO OE %d\n", TIAN_GPIO_OE2);
    err= gpio_request_one(TIAN_GPIO_OE2,
			GPIOF_OUT_INIT_LOW | GPIOF_EXPORT_DIR_FIXED, "OE");
	if (err)
		pr_err("mach-linino: error setting GPIO OE\n");
}


static void __init tian_setup_v4(void)
{
	u8 *art = (u8 *) KSEG1ADDR(0x1fff0000);
	static u8 mac[6];
    int r;
    void __iomem *reg;
    unsigned v;

	/* make lan / wan leds software controllable */
	ath79_gpio_output_select(TIAN_GPIO_LED0, AR934X_GPIO_OUT_GPIO);
	ath79_gpio_output_select(TIAN_GPIO_LED1, AR934X_GPIO_OUT_GPIO);
	ath79_gpio_output_select(TIAN_GPIO_LED2, AR934X_GPIO_OUT_GPIO);

	/* enable reset button */
	ath79_gpio_output_select(TIAN_GPIO_CONF_BTN, AR934X_GPIO_OUT_GPIO);
	ath79_gpio_function_enable(AR934X_GPIO_FUNC_JTAG_DISABLE);
	ath79_gpio_output_select(TIAN_GPIO_MCU_RESET, AR934X_GPIO_OUT_GPIO);

	/* UART1 (high-speed) configuration */
	r = gpio_request(TIAN_GPIO_UART1_TX, NULL);
	if (r) {
		pr_err("gpio_request failed on gpio %d: %d\n",
			TIAN_GPIO_UART1_TX, r);
		return;
	}
	gpio_direction_output(TIAN_GPIO_UART1_TX, 0);
	ath79_gpio_output_select(TIAN_GPIO_UART1_TX,
				AR934X_GPIO_UART1_TD_OUT);
	gpio_free(TIAN_GPIO_UART1_TX);

	r = gpio_request(TIAN_GPIO_UART1_RX, NULL);
	if (r) {
		pr_err("gpio_request failed on gpio %d: %d\n",
			TIAN_GPIO_UART1_RX, r);
		return;
	}
	gpio_direction_input(TIAN_GPIO_UART1_RX);
	gpio_free(TIAN_GPIO_UART1_TX);

	/* Mux for UART1 input: UART1 multiplexing is GPIO_IN_ENABLE9, see
	 * table 8-4 */
	reg = ath79_gpio_base + AR934X_GPIO_IN_ENABLE9;
	v = __raw_readl(reg);
	v &= ~0x00ff0000;
	v |= (TIAN_GPIO_UART1_RX << 16);
	__raw_writel(v, reg);

	/*custom gpios for Tian*/
	ath79_gpio_output_select(TIAN_GPIO_SWDIO, AR934X_GPIO_OUT_GPIO);
	ath79_gpio_output_select(TIAN_GPIO_SWDCLK, AR934X_GPIO_OUT_GPIO);

	ath79_register_m25p80(NULL);

	ath79_register_leds_gpio(-1, ARRAY_SIZE(tian_leds_gpio),
			tian_leds_gpio);
	ath79_register_gpio_keys_polled(-1, DS_KEYS_POLL_INTERVAL,
					 ARRAY_SIZE(tian_gpio_keys),
					 tian_gpio_keys);
	pr_info("mach-linino: enabling USB Controller");
	ath79_register_usb();

	ath79_init_mac(mac, art + DS_WMAC_MAC_OFFSET, 0);
	// mac[3] |= 0x08;
	mac[3] &= 0xF7;
	ath79_register_wmac(art + DS_CALDATA_OFFSET, mac);
	pr_info("%s-%d: wlan0 MAC:%02x:%02x:%02x:%02x:%02x:%02x\n", __FUNCTION__, __LINE__, mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);

	// mac[3] &= 0xF7;
	mac[3] |= 0x08;
	pr_info("%s-%d: eth0  MAC:%02x:%02x:%02x:%02x:%02x:%02x\n", __FUNCTION__, __LINE__, mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
	ap91_pci_init(art + DS_PCIE_CALDATA_OFFSET, mac);
	ath79_init_mac(ath79_eth0_data.mac_addr, mac, 0);

	ath79_setup_ar934x_eth_cfg(AR934X_ETH_CFG_RGMII_GMAC0 |
				   AR934X_ETH_CFG_SW_ONLY_MODE);

	ath79_register_mdio(1, 0x0);
	ath79_register_mdio(0, 0x0);

	mdiobus_register_board_info(db120_mdio0_info,
				    ARRAY_SIZE(db120_mdio0_info));

	printk(KERN_DEBUG "chow: Register Eth0\n");

	/* GMAC0 is connected to an AR8327 switch */
	ath79_eth0_data.phy_if_mode = PHY_INTERFACE_MODE_RGMII;
	ath79_eth0_data.phy_mask = BIT(0);
	ath79_eth0_data.mii_bus_dev = &ath79_mdio0_device.dev;
	ath79_eth0_pll_data.pll_1000 = 0x06000000;
	ath79_register_eth(0);

	/* enable OE of level shifters */
	ds_setup_level_shifter_oe();

	/* Register Software SPI controller */
	ds_register_spi();

	/* registering GPIO3 */
	gpio_request_one(TIAN_GPIO_GPIO3,
		GPIOF_OUT_INIT_LOW | GPIOF_EXPORT_DIR_FIXED,
		"GPIO3");
}

MIPS_MACHINE(ATH79_MACH_LININO_TIAN_V4, "linino-tian-v4", "Arduino Tian v4", tian_setup_v4);
