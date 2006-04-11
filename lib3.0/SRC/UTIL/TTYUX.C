
/* FILENAME ttyux.c
 *
 * Two C functions to do character I/O to the terminal (tty). 
 * The functions are called from Fortran on Unix systems.
 * They should not be mixed with Fortran terminal I/O statements.
 *
 * ttyput_  output a character string without a CR/LF
 * ttykey_  get a single character (key) without waiting for a CR,
 *          and without echoing it to the screen
 *
 * Portability --
 *
 * These functions are written to conform to POSIX.1 and ANSI C.
 * The major portability consideration is the calling interface
 * between Fortran and C, which may vary among vendors.  In this
 * implementation, the function names include a trailing "_" so they
 * can be called from Fortran, which appends a "_" to external names.
 * Other portability considerations are described in the in-line
 * comments of each function.  Many comments are to help Fortran
 * programmers understand C code!
 *
 * These functions do not interact with Fortran terminal I/O.  Fortran
 * terminal I/O is not specified by ANSI, but is vendor dependent.  Attempts
 * to mix these functions with READ and WRITE statements to the terminal
 * will result in unpredictable behavior.
 *
 * References --
 *
 * IEEE, 1988, IEEE Standard Portable Operating System Interface for Computer
 *   Environments: New York, IEEE, 317 p.
 *
 * Zlotnick, Fred, 1991, The POSIX.1 Standard, A Programmer's Guide: Redwood
 *   City, CA, The Benjamin/Cummings Publishing Company, Inc., 379 p.
 *
 * Implementation --
 *
 * Data General Aviion 300 workstation, DG/UX 4.32 operating system,
 * Green Hills Fortran, GNU C.
 *
 * History --
 *
 * Merritt Blalock, 10/26/91    - original coding
 * Merritt Blalock, 12/10/91    - ttykey works with input redirection
 * Merritt Blalock, 07/28/92    - delete ttyget and ttyfsh (unused) 
 * Merritt Blalock, 07/29/92    - workaround for 5.4 POSIX bug
 * Merritt Blalock, 04/14/93    - change putchar to write, fix buffering problem 
 * Jack Kittle,     07/14/94    - added ttytim for no wait get a character  
 */

#define _POSIX_SOURCE 1 /* tell header files to use POSIX (must come first) */
#include <signal.h>     /* interrupt signal header file */
#include <stdio.h>      /* standard I/O header file */
#include <termios.h>    /* terminal line control header file */

/* Function Declarations --
 *
 * "void" indicates the function returns no value
 */
   void ttyput_ (char *str, int len);
   void ttykey_ (char *chr, int len);
   void handlr  (int signal);
   void ttytim_ (char *chr, int len);

/* FUNCTION ttyput
 *
 * Purpose -- output a character string without a CR/LF
 *
 * Fortran Call --
 *   CHARACTER*n STR            <-- string written to terminal
 *   EXTERNAL TTYPUT
 *   CALL TTYPUT (STR(1:n))
 *
 * Portability --
 *
 * A Fortran subroutine call passes the address of an argument rather
 * than the value.  In this case, the address of "STR" is passed
 * to "ttyput," which receives it in the pointer, "str".  Fortran also
 * appends an additional argument, the length of "STR", which it
 * passes as a value, not an address!  The integer, "len", receives
 * this value.  Other vendor's Fortran may not do this.
 *
 * "str" is cast as an integer for use in "putchar".
 */

void ttyput_ (
   char *str, 	/* input  address of string being written to terminal */
   int len	/* input  length of string (supplied by Fortran, not user) */
   )
{
   write (1, str, (unsigned) len);
}


/* Static Variable Declaration --
 *
 * This declaration must come before the functions "ttykey" and "handlr".
 *
 * The static data structure defined here saves the normal terminal line
 * attributes.  The function "ttykey" changes some of the attributes in
 * order to get a single character.  If the user breaks out of the program
 * at that point, the function "handlr" restores the normal attributes using
 * this data structure.  The data structure is used like a Fortran COMMON
 * block to share data between functions.
 */

   static struct termios normal_termios;


/* FUNCTION ttykey
 *
 * Purpose -- get a single character (key) without waiting for a CR,
 *            and without echoing it to the screen
 *
 * Fortran Call --
 *   CHARACTER*1 CHR            <-- character read from keyboard
 *   EXTERNAL TTYKEY
 *   CALL TTYKEY(CHR)
 *
 * Portability --
 *
 * A Fortran subroutine call passes the address of an argument rather
 * than the value.  In this case, the address of "CHR " is passed to
 * "ttykey," which receives it in the pointer, "chr".  Fortran also
 * appends an additional argument, the length of "CHR", which it
 * passes as a value, not an address!  The integer, "len", receives
 * this value.  Other vendor's Fortran may not do this.
 *
 * "ttykey" places the keyboard character in the memory location pointed
 * to by "chr" and returns.  "len" is not modified.
 *
 * The calls to "sigemptyset", "sigaction", "tcgetattr", and "tcsetattr"
 * are POSIX.  This function first gets the normal terminal line control
 * attributes, saves them in a data structure, and defines a signal handler
 * in case the user breaks out of the function.  Then some of the normal line
 * attributes are changed so a single character can be read from the keyboard
 * without a CR.  When a character has been read, the normal line attributes
 * are restored.
 */

void ttykey_ (
   char *chr, 	/* output  address of character read from terminal */
   int len	/* output  length of string (for Fortran) */
   )
{

/* Establish data structures for normal and special line attributes. */

   extern struct termios normal_termios;
   struct termios special_termios;      /* for single-character mode */


/* Establish a handler for the keyboard interrupt signal. */

   extern void handlr (int);            /* handler function */
   struct sigaction sigact;             /* handler action data structure */
     sigact.sa_handler = handlr;        /*   name of handler */
     sigact.sa_flags = 0;               /*   not needed */
     sigemptyset(&sigact.sa_mask);      /*   not needed */


/* Save the normal line attributes and activate the interrupt handler. */

   tcgetattr(0, &normal_termios);       /* save the normal attributes */
   sigaction(SIGINT, &sigact, NULL);    /* now catch the interrupt signal */


/* Set the line attributes for single-character mode. */

   special_termios = normal_termios;    /* start with the normal mode */
     special_termios.c_iflag &= ~IGNBRK;/*   do not ignore BREAK */
     special_termios.c_iflag |= BRKINT; /*   BREAK generates an interrupt */
     special_termios.c_iflag &= ~INLCR; /*   do not map NL to CR */
     special_termios.c_iflag &= ~IXON;  /*   read ^S and ^Q */
     special_termios.c_lflag |= ISIG;   /*   check for INTR, QUIT, SUSP */
     special_termios.c_lflag &= ~ICANON;/*   do not wait for a CR */
     special_termios.c_lflag &= ~ECHO;  /*   do not echo */
     special_termios.c_cc[VMIN]  = 1;   /*   satisfy a read with 1 character */
     special_termios.c_cc[VTIME] = 0;   /*   no time limit on a read */

   tcsetattr(0, TCSANOW, &special_termios);     /* single-character mode */


/* Get the character and reset the line to normal mode. */

   *chr = (char) getchar();                     /* read a single character */
   tcsetattr(0, TCSANOW, &normal_termios);      /* normal mode */
}


/* FUNCTION handlr
 *
 * Purpose -- interrupt handler; restore normal terminal line attributes if
 *            "ttykey" receives an interrupt from the keyboard
 *
 * Portability --
 *
 * The call to "tcsetattr" is POSIX.  File descriptor 0 is standard input
 * (the terminal).  "TCSANOW" is defined in the header file, "termios.h",
 * to cause the terminal control attributes to be set immediately.
 *
 * If the keyboard interrupt (usually ^C or DEL) does not seem to work,
 * use the Unix "stty" command to determine what the interrupt key is,
 * or whether it is defined.
 */

void handlr (
   int signal	/* input  interrupt signal number, SIGINT */
   )
{
   extern struct termios normal_termios;

   tcsetattr(0, TCSANOW, &normal_termios);	/* normal mode */

   exit(1);
}

void ttytim_ (
   char *chr, 	/* output  address of character read from terminal */
   int len	/* output  length of string (for Fortran) */
   )
{

/* Establish data structures for normal and special line attributes. */

   extern struct termios normal_termios;
   struct termios special_termios;      /* for single-character mode */


/* Establish a handler for the keyboard interrupt signal. */

   extern void handlr (int);            /* handler function */
   struct sigaction sigact;             /* handler action data structure */
     sigact.sa_handler = handlr;        /*   name of handler */
     sigact.sa_flags = 0;               /*   not needed */
     sigemptyset(&sigact.sa_mask);      /*   not needed */


/* Save the normal line attributes and activate the interrupt handler. */

   tcgetattr(0, &normal_termios);       /* save the normal attributes */
   sigaction(SIGINT, &sigact, NULL);    /* now catch the interrupt signal */


/* Set the line attributes for single-character wait .1 second */

   special_termios = normal_termios;    /* start with the normal mode */
     special_termios.c_iflag &= ~IGNBRK;/*   do not ignore BREAK */
     special_termios.c_iflag |= BRKINT; /*   BREAK generates an interrupt */
     special_termios.c_iflag &= ~INLCR; /*   do not map NL to CR */
     special_termios.c_iflag &= ~IXON;  /*   read ^S and ^Q */
     special_termios.c_lflag |= ISIG;   /*   check for INTR, QUIT, SUSP */
     special_termios.c_lflag &= ~ICANON;/*   do not wait for a CR */
     special_termios.c_lflag &= ~ECHO;  /*   do not echo */
     special_termios.c_cc[VMIN]  = 0;   /*   only try with time */
     special_termios.c_cc[VTIME] = 0;   /*   only already availalbe char ret */

   tcsetattr(0, TCSANOW, &special_termios);     /* single-character mode */


/* Get the character and reset the line to normal mode. */

   *chr = (char) getchar();                     /* read a single character */
   tcsetattr(0, TCSANOW, &normal_termios);      /* normal mode */
}
