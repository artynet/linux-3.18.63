/*
 * --------------------------------------------------------------------
 * Core driver framework for ST NFC Transceiver
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

#include <linux/err.h>
#include <linux/init.h>
#include <linux/module.h>
#include <linux/netdevice.h>
#include <linux/nfc.h>
#include <linux/wait.h>
#include <net/nfc/digital.h>
#include <net/nfc/nfc.h>
#include "stnfcdev.h"

/* supported protocols */
#define DEVICE_SUPPORTED_PROT		(NFC_PROTO_ISO14443_MASK | \
					NFC_PROTO_ISO14443_B_MASK | \
					NFC_PROTO_ISO15693_MASK | \
					NFC_PROTO_MIFARE_MASK)
/* driver capabilities */
#define DEVICE_CAPABILITIES		NFC_DIGITAL_DRV_CAPS_IN_CRC

/* Command Send Interface */
/* DEVICE_COMMAND_SEND CMD Ids */
#define ECHO_CMD			0x55
#define WRITE_REGISTER_CMD		0x9
#define PROTOCOL_SELECT_CMD		0x2
#define SEND_RECEIVE_CMD		0x4

/* Select protocol codes */
#define ISO15693_PROTOCOL_CODE		0x1
#define ISO14443A_PROTOCOL_CODE		0x2
#define ISO14443B_PROTOCOL_CODE		0x3

/*
 * head room len is 2
 * 1 byte for cmd
 * 1 byte for size
 */
#define DEVICE_HEADROOM_LEN		2

/*
 * tailroom is 1 for ISO14443A
 * and 0 for ISO14443B/ISO15693,
 * hence the max value 1 should be
 * taken.
 */
#define DEVICE_TAILROOM_LEN		1

/* Command Response interface */
#define MAX_RESPONSE_BUFFER_SIZE	280
#define ECHORESPONSE			0x55
#define DEVICE_ERR_MASK			0xF
#define DEVICE_TIMEOUT_ERROR		0x87
#define DEVICE_NFCA_CRC_ERR_MASK	0x20
#define DEVICE_NFCB_CRC_ERR_MASK	0x01
#define TYPE2_RESP_ACK_NACK		0x90

/* device transmission flag values */
#define TRFLAG_NFCA_SHORT_FRAME		0x07
#define TRFLAG_NFCA_STD_FRAME		0x08
#define TRFLAG_NFCA_STD_FRAME_CRC	0x28

/* Misc defs */
#define HIGH				1
#define LOW				0
#define ISO14443A_RATS_REQ		0xE0
#define RATS_TB1_PRESENT_MASK		0x20
#define RATS_TA1_PRESENT_MASK		0x10
#define TB1_FWI_MASK			0xF0
#define WTX_REQ_FROM_TAG		0xF2

#define MAX_CMD_LEN			0x6

#define MAX_CMD_PARAMS			4

/* flag to differentiate synchronous & asynchronous request */
enum req_type {
	SYNC,
	ASYNC,
};

struct cmd {
	int cmd_len;
	unsigned char cmd_id;
	unsigned char no_cmd_params;
	unsigned char cmd_params[MAX_CMD_PARAMS];
	enum req_type req;
};

struct param_list {
	int param_offset;
	int new_param_val;
};

/*
 * List of top-level cmds to be used internally by the driver.
 * All these commands are build on top of basic commands
 * such as SEND_RECEIVE_CMD, PROTOCOL_SELECT_CMD, etc.
 * These top level cmds are used internally while implementing various ops of
 * digital layer/driver probe or extending the digital framework layer for
 * features that are not yet implemented there, for example, WTX cmd handling.
 */
enum nfcst_cmd_list {
	CMD_ECHO,
	CMD_NFCA_CONFIG,
	CMD_NFCA_DEMOGAIN,
	CMD_ISO14443B_DEMOGAIN,
	CMD_NFCA_PROTOCOL_SELECT,
	CMD_ISO14443B_PROTOCOL_SELECT,
	CMD_WTX_RESPONSE,
	CMD_FIELD_OFF,
	CMD_ISO15693_PROTOCOL_SELECT,
};

static const struct cmd cmd_array[] = {
	[CMD_ECHO] = {
		.cmd_len = 0x1,
		.cmd_id = ECHO_CMD,
		.no_cmd_params = 0,
		.req = SYNC,
	},
	[CMD_NFCA_CONFIG] = {
		.cmd_len = 0x6,
		.cmd_id = WRITE_REGISTER_CMD,
		.no_cmd_params = 0x4,
		.cmd_params = {0x3A, 0x00, 0x5A, 0x04},
		.req = SYNC,
	},
	[CMD_NFCA_DEMOGAIN] = {
		.cmd_len = 0x6,
		.cmd_id = WRITE_REGISTER_CMD,
		.no_cmd_params = 0x4,
		.cmd_params = {0x68, 0x01, 0x01, 0xDF},
		.req = SYNC,
	},
	[CMD_ISO14443B_DEMOGAIN] = {
		.cmd_len = 0x6,
		.cmd_id = WRITE_REGISTER_CMD,
		.no_cmd_params = 0x4,
		.cmd_params = {0x68, 0x01, 0x01, 0x51},
		.req = SYNC,
	},
	[CMD_NFCA_PROTOCOL_SELECT] = {
		.cmd_len = 0x6,
		.cmd_id = PROTOCOL_SELECT_CMD,
		.no_cmd_params = 0x4,
		.cmd_params = {ISO14443A_PROTOCOL_CODE, 0x00, 0x01, 0xA0},
		.req = SYNC,
	},
	[CMD_ISO14443B_PROTOCOL_SELECT] = {
		.cmd_len = 0x6,
		.cmd_id = PROTOCOL_SELECT_CMD,
		.no_cmd_params = 0x4,
		.cmd_params = {ISO14443B_PROTOCOL_CODE, 0x01, 0x03, 0xFF},
		.req = SYNC,
	},
	[CMD_WTX_RESPONSE] = {
		.cmd_len = 0x5,
		.cmd_id = SEND_RECEIVE_CMD,
		.no_cmd_params = 0x3,
		.cmd_params = {0xF2, 0x00, TRFLAG_NFCA_STD_FRAME_CRC},
		.req = ASYNC,
	},
	[CMD_FIELD_OFF] = {
		.cmd_len = 0x4,
		.cmd_id = PROTOCOL_SELECT_CMD,
		.no_cmd_params = 0x2,
		.cmd_params = {0x0, 0x0},
		.req = SYNC,
	},
	[CMD_ISO15693_PROTOCOL_SELECT] = {
		.cmd_len = 0x4,
		.cmd_id = PROTOCOL_SELECT_CMD,
		.no_cmd_params = 0x2,
		.cmd_params = {ISO15693_PROTOCOL_CODE, 0x0D},
		.req = SYNC,
	},
};

/* digital_cmd_complete_arg stores client context */
struct digital_cmd_complete_arg {
	nfc_digital_cmd_complete_t complete_cb;
	void *cb_usrarg;
	bool rats;
};

struct nfcst_context {
	struct nfc_digital_dev *ddev;
	struct nfc_dev *nfcdev;
	struct digital_cmd_complete_arg complete_cb_arg;
	unsigned char sendrcv_trflag;
	struct semaphore exchange_lock;
	u8 current_protocol;
	u8 current_rf_tech;
	int fwi;
	struct work_struct write_work;
	int wtx_mm;
	struct device *dev;
	void *phy_ctx;
	enum nfcst_phy phy;
	struct nfcst_if_ops *if_ops;
	struct nfcst_pltf_data cfg;
	bool req_is_sync;
	struct completion done;
	bool nfcst_power;
	bool trig_config;
	struct sk_buff *recv_buff;
};

static struct sk_buff *nfcst_send_recv_fr(struct nfcst_context *ctx,
					  struct sk_buff *send_buf,
					  enum req_type req)
{
	int err;

	if (!send_buf || !ctx || !ctx->if_ops->phy_send || !ctx->dev ||
	    !ctx->phy_ctx)
		return NULL;

	reinit_completion(&ctx->done);

	ctx->recv_buff = NULL;
	ctx->req_is_sync = req;

	err = ctx->if_ops->phy_send(ctx->phy_ctx, send_buf);
	if (err) {
		dev_err(ctx->dev, "phy_send error %d\n", err);
		return ERR_PTR(err);
	}

	if (req == ASYNC)
		return NULL;

	err = wait_for_completion_timeout(&ctx->done, msecs_to_jiffies(1000));
	if (!err) {
		dev_err(ctx->dev, "phy timeout error\n");
		return ERR_PTR(-ETIMEDOUT);
	}

	return ctx->recv_buff;
}

/*
 * nfcst_send_recv_cmd() is for sending commands to device
 * that are described in the cmd_array[]. It can optionally
 * receive the response if the cmd request is of type
 * SYNC.
 */
static int nfcst_send_recv_cmd(struct nfcst_context *context,
			       enum nfcst_cmd_list cmd,
			       int no_modif,
			       struct param_list *list_array)
{
	int i, ret = 0;
	struct sk_buff *skb_send, *skb_recv;

	if (cmd_array[cmd].cmd_len > MAX_CMD_LEN)
		return -EINVAL;
	if (cmd_array[cmd].no_cmd_params < no_modif)
		return -EINVAL;
	if (no_modif && !list_array)
		return -EINVAL;

	skb_send = alloc_skb(MAX_CMD_LEN, GFP_KERNEL);
	if (!skb_send)
		return -ENOMEM;

	skb_reserve(skb_send, cmd_array[cmd].cmd_len);
	skb_push(skb_send, cmd_array[cmd].cmd_len);

	skb_send->data[0] = cmd_array[cmd].cmd_id;
	if (cmd_array[cmd].no_cmd_params > 0) {
		skb_send->data[1] = cmd_array[cmd].no_cmd_params;
		memcpy(&skb_send->data[2], cmd_array[cmd].cmd_params,
		       cmd_array[cmd].no_cmd_params);
	}

	for (i = 0; i < no_modif; i++) {
		if (list_array[i].param_offset >= cmd_array[cmd].no_cmd_params)
			return -EINVAL;

		skb_send->data[2 + list_array[i].param_offset] =
						list_array[i].new_param_val;
	}

	skb_recv = nfcst_send_recv_fr(context, skb_send, cmd_array[cmd].req);
	if (IS_ERR(skb_recv)) {
		dev_err(context->dev, "send receive of frame fail with %ld\n",
			PTR_ERR(skb_recv));
		return PTR_ERR(skb_recv);
	}

	if (cmd_array[cmd].req == SYNC) {
		ret = -EIO;
		if (!skb_recv)
			return ret;

		if (cmd_array[cmd].cmd_id == ECHO_CMD) {
			if (skb_recv->data[0] != ECHORESPONSE)
				dev_err(context->dev, "echo failed, recv 0x%x\n",
					skb_recv->data[0]);
			else
				ret = 0;
		} else if (skb_recv->data[0]) {
			dev_err(context->dev, "send recv frame, err = 0x%x\n",
				skb_recv->data[0]);
		} else {
			ret = 0;
		}

		kfree_skb(skb_recv);
	}

	return ret;
}

static int secondary_configuration_type4a(struct nfcst_context *context)
{
	int result = 0;
	struct device *dev = &context->nfcdev->dev;

	/* 14443A config setting after select protocol */
	result = nfcst_send_recv_cmd(context,
				     CMD_NFCA_CONFIG,
				     0,
				     NULL);
	if (result) {
		dev_err(dev, "type a config cmd, err = 0x%x\n", result);
		return result;
	}

	/* 14443A demo gain setting */
	result = nfcst_send_recv_cmd(context,
				     CMD_NFCA_DEMOGAIN,
				     0,
				     NULL);
	if (result)
		dev_err(dev, "type a demogain cmd, err = 0x%x\n", result);

	return result;
}

static int secondary_configuration_type4b(struct nfcst_context *context)
{
	int result = 0;
	struct device *dev = &context->nfcdev->dev;

	result = nfcst_send_recv_cmd(context,
				     CMD_ISO14443B_DEMOGAIN,
				     0,
				     NULL);
	if (result)
		dev_err(dev, "type b demogain cmd, err = 0x%x\n", result);

	return result;
}

static int nfcst_select_protocol(struct nfcst_context *context, int type)
{
	int result = 0;
	struct device *dev;

	dev = &context->nfcdev->dev;

	switch (type) {
	case NFC_DIGITAL_RF_TECH_106A:
		context->current_rf_tech = NFC_DIGITAL_RF_TECH_106A;
		result = nfcst_send_recv_cmd(context,
					     CMD_NFCA_PROTOCOL_SELECT,
					     0,
					     NULL);
		if (result) {
			dev_err(dev, "protocol sel, err = 0x%x\n",
				result);
			return result;
		}

		/* secondary config. for 14443Type 4A after protocol select */
		result = secondary_configuration_type4a(context);
		if (result) {
			dev_err(dev, "type a secondary config, err = 0x%x\n",
				result);
			return result;
		}
		break;
	case NFC_DIGITAL_RF_TECH_106B:
		context->current_rf_tech = NFC_DIGITAL_RF_TECH_106B;
		result = nfcst_send_recv_cmd(context,
					     CMD_ISO14443B_PROTOCOL_SELECT,
					     0,
					     NULL);
		if (result) {
			dev_err(dev, "protocol sel send, err = 0x%x\n",
				result);
			return result;
		}

		/*
		 * delay of 5-6 ms is required after select protocol
		 * command in case of ISO14443 Type B
		 */
		usleep_range(50000, 60000);

		/* secondary config. for 14443Type 4B after protocol select */
		result = secondary_configuration_type4b(context);
		if (result) {
			dev_err(dev, "type b secondary config, err = 0x%x\n",
				result);
			return result;
		}
		break;
	case NFC_DIGITAL_RF_TECH_ISO15693:
		context->current_rf_tech = NFC_DIGITAL_RF_TECH_ISO15693;
		result = nfcst_send_recv_cmd(context,
					     CMD_ISO15693_PROTOCOL_SELECT,
					     0,
					     NULL);
		if (result) {
			dev_err(dev, "protocol sel send, err = 0x%x\n",
				result);
			return result;
		}
		break;
	default:
		return -EINVAL;
	}

	return 0;
}

static int iso14443_config_fdt(struct nfcst_context *context, int wtxm)
{
	int result = 0;
	struct nfc_digital_dev *nfcddev = context->ddev;
	unsigned char pp_typeb;
	struct param_list new_params[2];

	struct device *dev = &context->nfcdev->dev;

	pp_typeb = cmd_array[CMD_ISO14443B_PROTOCOL_SELECT].cmd_params[2];

	if (nfcddev->curr_protocol == NFC_PROTO_ISO14443 && context->fwi < 4)
		context->fwi = 4;

	new_params[0].param_offset = 2;
	if (nfcddev->curr_protocol == NFC_PROTO_ISO14443)
		new_params[0].new_param_val = context->fwi;
	else if (nfcddev->curr_protocol == NFC_PROTO_ISO14443_B)
		new_params[0].new_param_val = pp_typeb;

	new_params[1].param_offset = 3;
	new_params[1].new_param_val = wtxm;

	switch (nfcddev->curr_protocol) {
	case NFC_PROTO_ISO14443:
		result = nfcst_send_recv_cmd(context,
					     CMD_NFCA_PROTOCOL_SELECT,
					     2,
					     new_params);
		if (result) {
			dev_err(dev, "iso14443_config_fdt: nfcst_send_recv_cmd fails, err = %d\n",
				result);
			return result;
		}

		/* secondary config. for 14443Type 4A after protocol select */
		result = secondary_configuration_type4a(context);
		if (result) {
			dev_err(dev, "iso14443_config_fdt: secondary_configuration_type4a, err = %d\n",
				result);
			return result;
		}
		break;
	case NFC_PROTO_ISO14443_B:
		result = nfcst_send_recv_cmd(context,
					     CMD_ISO14443B_PROTOCOL_SELECT,
					     2,
					     new_params);
		if (result) {
			dev_err(dev, "WTX type b sel proto,err = %d\n", result);
			return result;
		}

		/* secondary config. for 14443Type 4B after protocol select */
		result = secondary_configuration_type4b(context);
		if (result) {
			dev_err(dev, "WTX type b second. config, err = 0x%x\n",
				result);
			return result;
		}
		break;
	default:
		return -EINVAL;
	}

	return 0;
}

static int nfcst_handle_config_fdt(struct nfcst_context *context,
				   bool new_wtx)
{
	int result = 0;
	unsigned char val_mm = 0;
	struct param_list new_params[1];
	struct nfc_digital_dev *nfcddev = context->ddev;
	struct device *dev = &context->nfcdev->dev;
	int wtx_val;

	if (new_wtx) {
		wtx_val = context->wtx_mm;
		result = iso14443_config_fdt(context, wtx_val & 0x3f);
		if (result) {
			dev_err(dev, "Config. setting error on WTX req, err = %d\n",
				result);
			return result;
		}

		/* Send response of wtx with ASYNC as no response expected */
		new_params[0].param_offset = 1;
		new_params[0].new_param_val = wtx_val;

		result = nfcst_send_recv_cmd(context,
					     CMD_WTX_RESPONSE,
					     1,
					     new_params);
		if (result)
			dev_err(dev, "WTX response send, err = %d\n", result);
		return result;
	}

	/* if no new wtx, cofigure with default values */
	if (nfcddev->curr_protocol == NFC_PROTO_ISO14443)
		val_mm = cmd_array[CMD_NFCA_PROTOCOL_SELECT].cmd_params[3];
	else if (nfcddev->curr_protocol == NFC_PROTO_ISO14443_B)
		val_mm = cmd_array[CMD_ISO14443B_PROTOCOL_SELECT].cmd_params[3];
	result = iso14443_config_fdt(context, val_mm);
	if (result)
		dev_err(dev, "Default config. setting error, err = %d\n",
			result);

	return result;
}

static void nfcst_handle_wtx(struct work_struct *work)
{
	int ret;
	struct nfcst_context *ctx = container_of(work,
						 struct nfcst_context,
						 write_work);

	struct device *dev = &ctx->nfcdev->dev;

	ret = nfcst_handle_config_fdt(ctx, true);
	if (ret)
		dev_err(dev, "nfcst_handle_wtx:: nfcst_handle_config_fdt, err = %d\n",
			ret);

	ctx->trig_config = true;
}

static int nfcst_error_handling(struct nfcst_context *context,
				struct sk_buff *skb_resp,
				int res_len)
{
	int result = 0;
	unsigned char error_byte;
	struct device *dev = &context->nfcdev->dev;

	/* First check device specific error */
	if (skb_resp->data[0] & DEVICE_ERR_MASK) {
		if (skb_resp->data[0] == DEVICE_TIMEOUT_ERROR)
			result = -ETIMEDOUT;
		else
			result = -EIO;
		dev_dbg(dev, "Error frame[0]: 0x%x and result %d\n",
			 skb_resp->data[0], result);
		return  result;
	}

	/* Check for CRC err only if CRC is present in the tag response */
	switch (context->current_rf_tech) {
	case NFC_DIGITAL_RF_TECH_106A:
		/* In case of Type2 ACK and NACK response, no CRC check*/
		if (context->ddev->curr_protocol == NFC_PROTO_MIFARE) {
			if (skb_resp->data[0] == TYPE2_RESP_ACK_NACK)
				break;
		}
		if (context->sendrcv_trflag == TRFLAG_NFCA_STD_FRAME_CRC) {
			error_byte = skb_resp->data[res_len - 3];
			if (error_byte & DEVICE_NFCA_CRC_ERR_MASK) {
				/* CRC error occurred */
				dev_err(dev, "CRC error, byte received = 0x%x\n",
					error_byte);
				result = -EIO;
			}
		}
		break;
	case NFC_DIGITAL_RF_TECH_106B:
	case NFC_DIGITAL_RF_TECH_ISO15693:
		error_byte = skb_resp->data[res_len - 1];
		if (error_byte & DEVICE_NFCB_CRC_ERR_MASK) {
			/* CRC error occurred */
			dev_err(dev, "CRC error, byte received = 0x%x\n",
				error_byte);
			result = -EIO;
		}
		break;
	}

	return result;
}

static void nfcst_response_handler(struct nfcst_context *context,
				   struct sk_buff *skb_resp)
{
	int skb_len;
	struct digital_cmd_complete_arg *cb_arg;

	cb_arg = &context->complete_cb_arg;

	/* Remove device specific header */
	skb_pull(skb_resp, 2);

	skb_len = skb_resp->len;

	/* check if it is case of RATS request reply & FWI is present */
	if (context->current_rf_tech == NFC_DIGITAL_RF_TECH_106A &&
	    cb_arg->rats && (skb_resp->data[1] & RATS_TB1_PRESENT_MASK)) {
		if (skb_resp->data[1] & RATS_TA1_PRESENT_MASK)
			context->fwi =
				(skb_resp->data[3] & TB1_FWI_MASK) >> 4;
		else
			context->fwi =
				(skb_resp->data[2] & TB1_FWI_MASK) >> 4;

		context->trig_config = true;
	}
	cb_arg->rats = false;

	/** CRC handling **/
	/* For MIFARE Type2, CRC handling will be done by digital framework */
	if (context->current_rf_tech == NFC_DIGITAL_RF_TECH_106A &&
	    context->ddev->curr_protocol == NFC_PROTO_MIFARE) {
		/* Removing Transceiver specific data */
		skb_trim(skb_resp, (skb_len - 3));
		return;
	}
	/* Remove CRC bytes only if received frames data has an eod (CRC) */
	switch (context->current_rf_tech) {
	case NFC_DIGITAL_RF_TECH_106A:
		if (context->sendrcv_trflag == TRFLAG_NFCA_STD_FRAME_CRC)
			skb_trim(skb_resp, (skb_len - 5));
		else
			skb_trim(skb_resp, (skb_len - 3));
		break;
	case NFC_DIGITAL_RF_TECH_106B:
	case NFC_DIGITAL_RF_TECH_ISO15693:
		skb_trim(skb_resp, (skb_len - 3));
		break;
	}
}

static int nfcst_wake(void *context)
{
	int nth_attempt = 1;
	int result;
	struct nfcst_context *ctx = (struct nfcst_context *)context;

	if (ctx->phy == PHY_UART) {
		do {
			result = nfcst_send_recv_cmd(ctx,
						     CMD_ECHO,
						     0,
						     NULL);
			if (!result) {
				ctx->nfcst_power = true;

				/*
				 * TBD: If baud rate is set different than
				 * the default, (i.e. nfcst_pltf_data contains
				 * a baudrate different than default), then
				 * we need to set the baudrate of our device.
				 * This is done by sending the 0xA command.
				 * The response of the command should be ignored
				 * because the response is received on the
				 * new baudrate and we have not yet
				 * re-configured our tty device on the new
				 * baudrate. So the received response of 0xA,
				 * which should be 0x55 (only one byte) could be
				 * some other value (so ignore it).
				 * Once the response of 1 byte is received, we
				 * must re-configure our tty device to the new
				 * baudrate.
				 * This must be done using the UART phy driver.
				 * For that we need to introduce a new ops
				 * (such as setconfig) and pass the
				 * nfcst_pltf_data struct saved in nfcst_context
				 * to it. SPI based phy driver could also need
				 * to implement this new ops for example to set
				 * some parameters specific to spi such as clk
				 * frequency (to be stored in nfcst_pltf_data)
				 */
				return 0;
			}
		} while (nth_attempt++ < 4);
	} else { /* TBD: Support for SPI interface */
		return -EINVAL;
	}

	return -ETIMEDOUT;
}

/* NFC ops functions definition */
static int nfcst_in_configure_hw(struct nfc_digital_dev *ddev, int type,
				 int param)
{
	struct nfcst_context *context = nfc_digital_get_drvdata(ddev);

	if (!context->nfcst_power)
		return -EIO;

	if (type == NFC_DIGITAL_CONFIG_RF_TECH)
		return nfcst_select_protocol(context, param);

	if (type == NFC_DIGITAL_CONFIG_FRAMING) {
		switch (param) {
		case NFC_DIGITAL_FRAMING_NFCA_SHORT:
			context->sendrcv_trflag = TRFLAG_NFCA_SHORT_FRAME;
			break;
		case NFC_DIGITAL_FRAMING_NFCA_STANDARD:
			context->sendrcv_trflag = TRFLAG_NFCA_STD_FRAME;
			break;
		case NFC_DIGITAL_FRAMING_NFCA_T4T:
		case NFC_DIGITAL_FRAMING_NFCA_T2T:
		case NFC_DIGITAL_FRAMING_NFCA_NFC_DEP:
		case NFC_DIGITAL_FRAMING_NFCA_STANDARD_WITH_CRC_A:
			context->sendrcv_trflag = TRFLAG_NFCA_STD_FRAME_CRC;
			break;
		case NFC_DIGITAL_FRAMING_NFCB:
		case NFC_DIGITAL_FRAMING_ISO15693_INVENTORY:
		case NFC_DIGITAL_FRAMING_ISO15693_T5T:
			break;
		}
	}

	return 0;
}

static int rf_off(struct nfcst_context *context)
{
	int rc;
	struct device *dev;

	dev = &context->nfcdev->dev;

	/*
	 * TBD: Currently basic pwr mgmt. is implemented !
	 * (switching off the field)
	 * In a better approach, we can send the idle command
	 * after switching off the field. This will save
	 * more power.
	 * In fact even better is to switch off the device
	 * assuming the power to device is coming from a
	 * regulator which can be turned off/on.
	 */
	rc = nfcst_send_recv_cmd(context, CMD_FIELD_OFF, 0, NULL);
	if (rc)
		dev_err(dev, "protocol sel send field off, err = %d\n", rc);

	return rc;
}

static int nfcst_in_send_cmd(struct nfc_digital_dev *ddev,
			     struct sk_buff *skb,
			     u16 timeout,
			     nfc_digital_cmd_complete_t cb,
			     void *arg)
{
	struct nfcst_context *context = nfc_digital_get_drvdata(ddev);
	int rc;
	int len_data_to_tag = 0;

	if (!context->nfcst_power)
		return -EIO;

	/*
	 * down the semaphore to indicate that last nfcst_in_send_cmd()
	 * call is pending, If interrupted, WARN and return !
	 */
	rc = down_killable(&context->exchange_lock);
	if (rc) {
		WARN(1, "Semaphore wait is interrupted in nfcst_in_send_cmd\n");
		return rc;
	}

	if (context->trig_config) {
		context->trig_config = false;
		rc = nfcst_handle_config_fdt(context, false);
		if (rc) {
			dev_err(&context->nfcdev->dev, "config fdt failed from nfcst_in_send_cmd %d\n",
				rc);
			return rc;
		}
	}

	switch (context->current_rf_tech) {
	case NFC_DIGITAL_RF_TECH_106A:
		len_data_to_tag = skb->len + 1;
		*skb_put(skb, 1) = context->sendrcv_trflag;
		break;
	case NFC_DIGITAL_RF_TECH_106B:
	case NFC_DIGITAL_RF_TECH_ISO15693:
		len_data_to_tag = skb->len;
		break;
	default:
		return -EINVAL;
	}

	skb_push(skb, 2);
	skb->data[0] = SEND_RECEIVE_CMD;
	skb->data[1] = len_data_to_tag;

	context->complete_cb_arg.cb_usrarg = arg;
	context->complete_cb_arg.complete_cb = cb;

	if ((skb->data[2] == ISO14443A_RATS_REQ) &&
	    context->current_rf_tech == NFC_DIGITAL_RF_TECH_106A)
		context->complete_cb_arg.rats = true;

	rc = PTR_ERR(nfcst_send_recv_fr(context, skb, ASYNC));
	if (rc) {
		dev_err(&context->nfcdev->dev,
			"Error %d trying to perform data_exchange", rc);
		up(&context->exchange_lock);
	}

	return rc;
}

/* p2p will be supported in a later release ! */
static int nfcst_tg_configure_hw(struct nfc_digital_dev *ddev,
				 int type,
				 int param)
{
	return 0;
}

static int nfcst_tg_send_cmd(struct nfc_digital_dev *ddev,
			     struct sk_buff *skb,
			     u16 timeout,
			     nfc_digital_cmd_complete_t cb,
			     void *arg)
{
	return 0;
}

static int nfcst_tg_listen(struct nfc_digital_dev *ddev,
			   u16 timeout,
			   nfc_digital_cmd_complete_t cb,
			   void *arg)
{
	return 0;
}

static int nfcst_tg_get_rf_tech(struct nfc_digital_dev *ddev, u8 *rf_tech)
{
	return 0;
}

static int nfcst_switch_rf(struct nfc_digital_dev *ddev, bool on)
{
	u8 rf_tech;
	int rc;

	struct nfcst_context *context = nfc_digital_get_drvdata(ddev);

	if (!context->nfcst_power) {
		if (on) {
			rc = nfcst_wake(context);
			if (rc)
				return rc;
		} else {
			return 0;
		}
	}

	rf_tech = ddev->curr_rf_tech;

	if (on)
		/* switch on RF field */
		return nfcst_select_protocol(context, rf_tech);

	/* switch OFF RF field */
	return rf_off(context);
}

/* TODO nfcst_abort_cmd */
static void nfcst_abort_cmd(struct nfc_digital_dev *ddev)
{
}

static struct nfc_digital_ops nfcst_nfc_digital_ops = {
	.in_configure_hw = nfcst_in_configure_hw,
	.in_send_cmd = nfcst_in_send_cmd,

	.tg_listen = nfcst_tg_listen,
	.tg_configure_hw = nfcst_tg_configure_hw,
	.tg_send_cmd = nfcst_tg_send_cmd,
	.tg_get_rf_tech = nfcst_tg_get_rf_tech,

	.switch_rf = nfcst_switch_rf,
	.abort_cmd = nfcst_abort_cmd,
};

/*
 * phy drivers can register with the framework,
 * this will return the new (attached) nfcst_context object
 */
void *nfcst_register_phy(enum nfcst_phy phy,
			 void *phy_ctx,
			 struct nfcst_if_ops *ops,
			 struct device *dev,
			 struct nfcst_pltf_data *pdata)
{
	int ret;
	struct nfcst_context *ctx;

	ctx = kzalloc(sizeof(*ctx), GFP_KERNEL);
	if (!ctx)
		return ERR_PTR(-ENOMEM);

	ctx->phy_ctx = phy_ctx;
	ctx->if_ops = ops;
	ctx->dev = dev;
	ctx->phy = phy;

	memcpy(&ctx->cfg, pdata, sizeof(*pdata));

	ctx->fwi = cmd_array[CMD_NFCA_PROTOCOL_SELECT].cmd_params[2];

	init_completion(&ctx->done);

	/* create NFC dev object and register with NFC Subsystem */
	ctx->ddev = nfc_digital_allocate_device(&nfcst_nfc_digital_ops,
						DEVICE_SUPPORTED_PROT,
						DEVICE_CAPABILITIES,
						DEVICE_HEADROOM_LEN,
						DEVICE_TAILROOM_LEN);
	if (!ctx->ddev) {
		ret = -ENOMEM;
		goto error;
	}

	ctx->nfcdev = ctx->ddev->nfc_dev;
	nfc_digital_set_parent_dev(ctx->ddev, dev);

	ret = nfc_digital_register_device(ctx->ddev);
	if (ret) {
		dev_err(&ctx->nfcdev->dev, "st nfc device registration failed\n");
		goto err_free_digital_device;
	}

	nfc_digital_set_drvdata(ctx->ddev, ctx);

	sema_init(&ctx->exchange_lock, 1);
	INIT_WORK(&ctx->write_work, nfcst_handle_wtx);

	return (void *)ctx;

err_free_digital_device:
	nfc_digital_free_device(ctx->ddev);

error:
	kfree(ctx);
	return ERR_PTR(ret);
}
EXPORT_SYMBOL_GPL(nfcst_register_phy);

void nfcst_unregister_phy(void *context)
{
	struct nfcst_context *ctx = (struct nfcst_context *)context;

	up(&ctx->exchange_lock);
	nfc_digital_unregister_device(ctx->ddev);
	nfc_digital_free_device(ctx->ddev);
	kfree(ctx);
}
EXPORT_SYMBOL_GPL(nfcst_unregister_phy);

int nfcst_recv_frame(void *context, struct sk_buff *skb_res)
{
	int result;
	struct digital_cmd_complete_arg *cb_arg;

	struct nfcst_context *ctx = (struct nfcst_context *)context;

	/* first check the sync request */
	if (ctx->req_is_sync == SYNC) {
		ctx->recv_buff = skb_res;
		complete(&ctx->done);
		return 0;
	}

	/* now check for async request */
	cb_arg = &ctx->complete_cb_arg;

	result = nfcst_error_handling(ctx, skb_res, skb_res->len);
	if (result)
		goto end;

	if (skb_res->len > 2 && skb_res->data[2] == WTX_REQ_FROM_TAG) {
		ctx->wtx_mm = skb_res->data[3];
		kfree_skb(skb_res);
		schedule_work(&ctx->write_work);
		return 0;
	}

	nfcst_response_handler(ctx, skb_res);

	/* call digital layer callback */
	cb_arg->complete_cb(ctx->ddev, cb_arg->cb_usrarg, skb_res);

	/* up the semaphore before returning */
	up(&ctx->exchange_lock);

	return 0;

end:
	kfree_skb(skb_res);
	cb_arg->rats = false;
	skb_res = ERR_PTR(result);
	/* callback with error */
	cb_arg->complete_cb(ctx->ddev, cb_arg->cb_usrarg, skb_res);
	/* up the semaphore before returning */
	up(&ctx->exchange_lock);

	/*
	 * frame is received with success, though frame itself can
	 * contain error or new error generated. In later case we
	 * already inform the digital layer about the error, but
	 * the underlying phy layer must be reported as no error
	 */
	return 0;
}
EXPORT_SYMBOL_GPL(nfcst_recv_frame);

int nfcst_recv_fr_len(void *context, const u8 *hdr, int count)
{
	/* if echo response, nothing to read after */
	if (hdr[0] == ECHORESPONSE)
		return 1;

	if (hdr[0] & 0x60)
		return (((hdr[0] & 0x60) >> 5) << 8) | hdr[1];
	else
		return hdr[1];
}
EXPORT_SYMBOL_GPL(nfcst_recv_fr_len);

int nfcst_recv_hdr_len(void *context, const u8 *data, int count)
{
	if (data[0] == ECHORESPONSE)
		return 0;

	return DEVICE_HEADROOM_LEN;
}
EXPORT_SYMBOL_GPL(nfcst_recv_hdr_len);

int nfcst_recv_max_fr_sz(void *context)
{
	return MAX_RESPONSE_BUFFER_SIZE;
}
EXPORT_SYMBOL_GPL(nfcst_recv_max_fr_sz);

struct nfcst_pltf_data *nfcst_pltf_data(void *context)
{
	struct nfcst_context *ctx = (struct nfcst_context *)context;

	return &ctx->cfg;
}
EXPORT_SYMBOL_GPL(nfcst_pltf_data);

MODULE_AUTHOR("Shikha Singh <shikha.singh@st.com>");
MODULE_AUTHOR("Sudeep Biswas <sudeep.biswas@st.com>");
MODULE_DESCRIPTION("ST NFC Transceiver device driver core");
MODULE_LICENSE("GPL v2");
