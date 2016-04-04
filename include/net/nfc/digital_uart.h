/*
 * NFC Digital UART Line Discipline Framework API
 *
 * Copyright (c) 2016, STMicroelectronics Pvt. Ltd.
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 */

#ifndef __NFC_DIGITAL_UART_H
#define __NFC_DIGITAL_UART_H

#include <linux/tty.h>
#include <linux/skbuff.h>

/**
 * Drivers that can register with DIGITAL UART LDISC Framework
 */
enum digital_uart_driver {
	DIGITAL_UART_DRIVER_ST = 0,
	DIGITAL_UART_DRIVER_MAX,
};

enum digital_uart_stop_bits {
	DIGITAL_UART_STOP_BIT_1 = 0,
	DIGITAL_UART_STOP_BIT_2,
};

struct digital_uart;

#define DIGITALUARTSETDRIVER	_IOW('V', 0, char *)

/*
 * DIGITAL UART LDISC Framework ops
 */
struct digital_uart_ops {
	/* called when SETDRIVER IOCTL called on tty */
	int (*open)(struct digital_uart *du);
	/* called when ldisc of tty changed to other than N_DIGITAL */
	void (*close)(struct digital_uart *du);
	/* called after a frame is received */
	int (*recv)(struct digital_uart *du, struct sk_buff *skb);
	/* called by low level uart driver's tty layer when the data is
	 * received at UART port.
	 */
	int (*recv_buff)(struct digital_uart *du, const u8 *data, char *flags,
			 int count);
	/* called for sending a frame through UART */
	int (*send)(struct digital_uart *du, struct sk_buff *skb);
	/* if required to do some special handling before starting the
	 * transmission */
	void (*tx_start)(struct digital_uart *du);
	/* if required to do some special handling before ending the
	 * transmission */
	void (*tx_done)(struct digital_uart *du);
	/* returns the received frame header size */
	/* called at the time of received frame processing */
	int (*recv_fr_hdr_sz)(struct digital_uart *du,
			      const u8 *data, int count);
	/* returns the max size of frame that can be received*/
	int (*recv_fr_max_sz)(struct digital_uart *du);
	/* return payload size */
	int (*recv_pl_sz)(struct digital_uart *du, const u8 *data, int count);
};

struct digital_uart {
	struct module *owner;
	struct digital_uart_ops ops;
	const char *name;
	enum digital_uart_driver driver;
	/* Pointer of corresponding TTY */
	struct tty_struct *tty;
	/* worker for writing the frame */
	struct work_struct write_work;
	/* lock to serialize the frame read operation */
	spinlock_t rx_lock;
	unsigned long tx_state;
	/* queue of frames to be written */
	struct sk_buff_head tx_q;
	/* current write frame */
	struct sk_buff *tx_skb;
	/* current read frame */
	struct sk_buff *rx_skb;
	/* store received pkt length in ldisc context */
	int rx_packet_len;
	/* store drive specific data */
	void *drv_data;
};

/* exported helper functions */
int digital_uart_register(struct digital_uart *du);
void digital_uart_unregister(struct digital_uart *du);
void digital_uart_set_config(struct digital_uart *du, int baudrate,
			     int flow_ctrl,
			     enum digital_uart_stop_bits stop_bits);

#endif /* __NFC_DIGITAL_UART_H */
