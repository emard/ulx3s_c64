# pinout that should work on the same bitstream with different esp32 pinout
# FPGA   v2.1.x       v3.0.x       v3.1.7
# L1     wifi_gpio16  wifi_gpio16  wifi_gpio26
# L2     wifi_gpio0   wifi_gpio0   wifi_gpio22
# N3     wifi_gpio17  wifi_gpio17  wifi_gpio27
# N4     wifi_gpio5   wifi_gpio5   wifi_gpio19
# G5     wifi_gpio34  wifi_gpio35  wifi_gpio39
# H1     wifi_gpio4   wifi_gpio4   wifi_gpio4
# K1     wifi_gpio12  wifi_gpio12  wifi_gpio12

# v2.1.2 v3.0.x
gpio_cs   = const(5)
gpio_sck  = const(16)
gpio_miso = const(12)
gpio_mosi = const(4)
gpio_irq  = const(0)

# v3.1.6 v3.1.7
#gpio_cs   = const(19)
#gpio_sck  = const(26)
#gpio_miso = const(12)
#gpio_mosi = const(4)
#gpio_irq  = const(22)
