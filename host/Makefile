TARGET = hidtool
OBJS = hidtool.o hiddata.o
USBFLAGS=   `libusb-config --cflags`
USBLIBS=    `libusb-config --libs`
CFLAGS = -Wall -O0 -g $(USBFLAGS)
LDFLAGS = $(USBLIBS)
all: $(TARGET)
$(TARGET): $(OBJS)
	gcc $(LDFLAGS) -o $(TARGET) $(OBJS)
