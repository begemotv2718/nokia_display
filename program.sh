until timeout 30 avrdude  -p atmega8 -b 9600 -i 50   -P /dev/ttyS0  -c nikolaew   -U flash:w:main.hex
do
  ((c++))
  echo -n -e "\nrepeat\n"
  echo "Attempt $c"
done
