/*
 * --------------------------------------------------------------------
 * LDisc UART Driver for ST NFC Transceiver
 * --------------------------------------------------------------------
 * Copyright (C) 2015 STMicroelectronics Pvt. Ltd. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#include <linux/module.h>
#include <linux/of.h>
#include <net/nfc/digital_uart.h>
#include "stnfcdev.h"

#define DEVICE_DEFAULT_BAUD 57600

static unsigned int baud = DEVICE_DEFAULT_BAUD;

static int nfcst_uart_digital_send(void *phy_ctx, struct sk_buff *skb)
{
	struct digital_uart *du = (struct digital_uart *)phy_ctx;

	return du->ops.send(du, skb);
}

static struct nfcst_if_ops uart_ops = {
	.phy_send = nfcst_uart_digital_send,
};

static int nfcst_uart_parse_dt(struct device_node *node,
			       struct nfcst_pltf_data *pdata)
{
	struct device_node *matched_node;
	const void *ptr;

	matched_node = of_find_compatible_node(node, NULL, "st,nfc-uart");
	if (!matched_node)
		return -ENODEV;

	ptr = of_get_property(matched_node, "st,baudrate", NULL);
	if (!ptr)
		return -ENODEV;

	pdata->baudrate = be32_to_cpup(ptr);

	return 0;
}

/*
** DIGITAL UART OPS
*/
static int nfcst_digital_uart_open(struct digital_uart *du)
{
	void *priv;
	int err;
	struct nfcst_pltf_data config;
	struct nfcst_pltf_data *pdata = NULL;

	if (du->tty->dev->parent && du->tty->dev->parent->of_node) {
		err = nfcst_uart_parse_dt(du->tty->dev->parent->of_node,
					  &config);
		if (err)
			dev_err(du->tty->dev, "No st nfc uart platform data found in DT\n");
		else
			pdata = &config;
	}

	if (!pdata) {
		dev_info(du->tty->dev, "No platform data / DT -> fallback to module params\n");
		config.baudrate = baud;
		pdata = &config;
	}
	priv = nfcst_register_phy(PHY_UART, (void *)du, &uart_ops,
				  du->tty->dev, pdata);
	if (IS_ERR(priv))
		return PTR_ERR(priv);

	du->drv_data = priv;

	/* set the default configuration */
	digital_uart_set_config(du,
				DEVICE_DEFAULT_BAUD,
				0,
				DIGITAL_UART_STOP_BIT_2);

	return 0;
}

static void nfcst_digital_uart_close(struct digital_uart *du)
{
	nfcst_unregister_phy(du->drv_data);
}

static int nfcst_digital_uart_recv(struct digital_uart *du, struct sk_buff *skb)
{
	return nfcst_recv_frame(du->drv_data, skb);
}

static int nfcst_digital_uart_recv_hdr_len(struct digital_uart *du,
					   const u8 *data,
					   int count)
{
	return nfcst_recv_hdr_len(du->drv_data, data, count);
}

static int nfcst_digital_uart_fr_len(struct digital_uart *du,
				     const u8 *data,
				     int count)
{
	return nfcst_recv_fr_len(du->drv_data, data, count);
}

static int nfcst_digital_uart_max_fr_sz(struct digital_uart *du)
{
	return nfcst_recv_max_fr_sz(du->drv_data);
}

static struct digital_uart nfcst_nci_uart = {
	.owner  = THIS_MODULE,
	.name   = "nfcst_uart",
	.driver = DIGITAL_UART_DRIVER_ST,
	.ops	= {
		.open		= nfcst_digital_uart_open,
		.close		= nfcst_digital_uart_close,
		.recv		= nfcst_digital_uart_recv,
		.recv_fr_hdr_sz	= nfcst_digital_uart_recv_hdr_len,
		.recv_pl_sz	= nfcst_digital_uart_fr_len,
		.recv_fr_max_sz	= nfcst_digital_uart_max_fr_sz,
	}
};

/*
** Module init
*/
static int nfcst_uart_init_module(void)
{
	return digital_uart_register(&nfcst_nci_uart);
}

static void nfcst_uart_exit_module(void)
{
	digital_uart_unregister(&nfcst_nci_uart);
}

module_init(nfcst_uart_init_module);
module_exit(nfcst_uart_exit_module);

MODULE_AUTHOR("Sudeep Biswas <sudeep.biswas@st.com>");
MODULE_DESCRIPTION("ST NFC-over-UART");
MODULE_LICENSE("GPL v2");

module_param(baud, uint, 0);
MODULE_PARM_DESC(baud, "Tell the UART baudrate for communication");
