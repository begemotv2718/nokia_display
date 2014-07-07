/* Name: hidtool.c
 * Project: hid-data example
 * Author: Christian Starkjohann
 * Creation Date: 2008-04-11
 * Tabsize: 4
 * Copyright: (c) 2008 by OBJECTIVE DEVELOPMENT Software GmbH
 * License: GNU GPL v2 (see License.txt), GNU GPL v3 or proprietary (CommercialLicense.txt)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hiddata.h"
#include "../usbconfig.h"  /* for device VID, PID, vendor name and product name */

/* ------------------------------------------------------------------------- */

static char *usbErrorMessage(int errCode)
{
static char buffer[96];

    switch(errCode){
        case USBOPEN_ERR_ACCESS:      return "Access to device denied";
        case USBOPEN_ERR_NOTFOUND:    return "The specified device was not found";
        case USBOPEN_ERR_IO:          return "Communication error with device";
        default:
            sprintf(buffer, "Unknown USB error %d", errCode);
            return buffer;
    }
    return NULL;    /* not reached */
}

static usbDevice_t  *openDevice(void)
{
usbDevice_t     *dev = NULL;
unsigned char   rawVid[2] = {USB_CFG_VENDOR_ID}, rawPid[2] = {USB_CFG_DEVICE_ID};
char            vendorName[] = {USB_CFG_VENDOR_NAME, 0}, productName[] = {USB_CFG_DEVICE_NAME, 0};
int             vid = rawVid[0] + 256 * rawVid[1];
int             pid = rawPid[0] + 256 * rawPid[1];
int             err;

    if((err = usbhidOpenDevice(&dev, vid, vendorName, pid, productName, 0)) != 0){
        fprintf(stderr, "error finding %s: %s\n", productName, usbErrorMessage(err));
        return NULL;
    }
    return dev;
}

/* ------------------------------------------------------------------------- */

static void hexdump(char *buffer, int len)
{
int     i;
FILE    *fp = stdout;

    for(i = 0; i < len; i++){
        if(i != 0){
            if(i % 16 == 0){
                fprintf(fp, "\n");
            }else{
                fprintf(fp, " ");
            }
        }
        fprintf(fp, "0x%02x", buffer[i] & 0xff);
    }
    if(i != 0)
        fprintf(fp, "\n");
}

static int  hexread(char *buffer, char *string, int buflen)
{
char    *s;
int     pos = 0;

    while((s = strtok(string, ", ")) != NULL && pos < buflen){
        string = NULL;
        buffer[pos++] = (char)strtol(s, NULL, 0);
    }
    return pos;
}

/* ------------------------------------------------------------------------- */

static void usage(char *myName)
{
    fprintf(stderr, "usage:\n");
    fprintf(stderr, "  %s read\n", myName);
    fprintf(stderr, "  %s display <x> <y> \"<string>\"\n", myName);
    fprintf(stderr, "  %s contrast <vop> <bias>\n", myName);
}

int main(int argc, char **argv)
{
usbDevice_t *dev;
char        buffer[129];    /* room for dummy report ID */
int         err;

    if(argc < 2){
        usage(argv[0]);
        exit(1);
    }
    if((dev = openDevice()) == NULL)
        exit(1);
    if(strcasecmp(argv[1], "read") == 0){
        int len = sizeof(buffer);
        if((err = usbhidGetReport(dev, 0, buffer, &len)) != 0){
            fprintf(stderr, "error reading data: %s\n", usbErrorMessage(err));
        }else{
            hexdump(buffer + 1, sizeof(buffer) - 1);
        }
    }else if(strcasecmp(argv[1], "display") == 0){
        int x,y;
        if(argc<5){
          usage(argv[0]);
          exit(1);
        }
        memset(buffer, 0, sizeof(buffer));
        x=strtol(argv[2],NULL,10);
        y=strtol(argv[3],NULL,10);
        if( x<0 || x>76 || y<0 || y> 5){
           usage(argv[0]);
           exit(1);
        }
       
        snprintf(&buffer[3], 17,argv[4]);
        buffer[0]=REPORT_DISPLAY;
        buffer[1]=(char)x;
        buffer[2]=(char)y;
        if((err = usbhidSetReport(dev, buffer, 19)) != 0)   /* add a dummy report ID */
            fprintf(stderr, "error writing data: %s\n", usbErrorMessage(err));
    }else if(strcasecmp(argv[1],"contrast") == 0){
        int vop,bias;
        if(argc<4){
          usage(argv[0]);
          exit(1);
        }
        memset(buffer, 0, sizeof(buffer));
        vop=strtol(argv[2],NULL,10);
        bias=strtol(argv[3],NULL,10);
        if( vop<0 || vop>0x7f || bias<0 || bias> 7){
           printf("values of vop or bias out of range\n"); 
           exit(1);
        }
       
        buffer[0]=REPORT_SET_CONTRAST;
        buffer[1]=(char)vop;
        buffer[2]=(char)bias;
        if((err = usbhidSetReport(dev, buffer, 3)) != 0)   /* add a dummy report ID */
            fprintf(stderr, "error writing data: %s\n", usbErrorMessage(err));
    }else{
        usage(argv[0]);
        exit(1);
    }
    usbhidCloseDevice(dev);
    return 0;
}

/* ------------------------------------------------------------------------- */
