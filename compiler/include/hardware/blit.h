#ifndef HARDWARE_BLIT_H
#define HARDWARE_BLIT_H

/*
    (C) 1997 AROS - The Amiga Replacement OS
    $Id$

    Desc: Amiga bit blitter
    Lang: english
*/

/* Note: bare support for needed stuff. This file needs to be completed! */

/* blitter minterms */
#define ABC     0x80
#define ABNC    0x40
#define ANBC    0x20
#define ANBNC   0x10
#define NABC    0x08
#define NABNC   0x04
#define NANBC   0x02
#define NANBNC  0x01

/* common minterm operations */
#define A_XOR_C   NABC|ABNC|NANBC|ANBNC

#endif /* HARDWARE_BLIT_H */
