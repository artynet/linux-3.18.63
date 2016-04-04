/*
 * --------------------------------------------------------------------
 * API defination provided by ST NFC Transceiver core framework
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

#ifndef _STNFCDEV_H_
#define _STNFCDEV_H_

/* phy type */
enum nfcst_phy {
	PHY_SPI = 0,
	PHY_UART,
};

struct nfcst_pltf_data {
	/* UART specific */
	unsigned int baudrate;

	/* TBD: SPI specific */

	/*
	 * TBD: Generic stuff that can be required for any interface
	 * such as regulator (device can receive power from a
	 * regulator which needs to be enabled in the device
	 * wake-up. Also it can help power mgmt., for example
	 * switching off the regulator when the device is
	 * disabled (off) and then switching it on when the
	 * device is again enabled (on))
	 */
};

struct nfcst_if_ops {
	int (*phy_send)(void *phy_ctx, struct sk_buff *skb);
};

void *nfcst_register_phy(enum nfcst_phy phy,
			 void *phy_ctx,
			   struct nfcst_if_ops *ops,
			   struct device *dev,
			   struct nfcst_pltf_data *pdata);

void nfcst_unregister_phy(void *context);
int nfcst_recv_frame(void *context, struct sk_buff *skb_res);
int nfcst_recv_fr_len(void *context, const u8 *data, int count);
int nfcst_recv_hdr_len(void *context, const u8 *data, int count);
int nfcst_recv_max_fr_sz(void *context);
struct nfcst_pltf_data *nfcst_pltf_data(void *context);

#endif
