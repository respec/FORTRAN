#include <string.h>

    void   copyi_ ( arLen, arIn, arOut)

    int         *arLen;
    char        *arIn, *arOut;

    {
        int  Len;
        Len = (*arLen)*4;
        (void) memcpy (arOut, arIn, Len);
    }

    void   copyr_ ( arLen, arIn, arOut)

    int         *arLen;
    char        *arIn, *arOut;

    {
        int  Len;

        Len = (*arLen)*4;
        (void) memcpy (arOut, arIn, Len);
    }

    void   copyd_ ( arLen, arIn, arOut)

    int         *arLen;
    char        *arIn, *arOut;

    {
        int  Len;

        Len = (*arLen)*8;
        (void) memcpy (arOut, arIn, Len);
    }
