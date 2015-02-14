# kimim-emacs

# cygwin configuration

Because recent version of cygwin requires security check, make cygserver as a service.
~~~
kma@ABB /usr/bin
$ ./cygserver-config
Generating /etc/cygserver.conf file


Warning: The following function requires administrator privileges!

Do you want to install cygserver as service?
(Say "no" if it's already installed as service) (yes/no) yes

The service has been installed under LocalSystem account.
To start it, call `net start cygserver' or `cygrunsrv -S cygserver'.

Further configuration options are available by editing the configuration
file /etc/cygserver.conf.  Please read the inline information in that
file carefully. The best option for the start is to just leave it alone.

Basic Cygserver configuration finished. Have fun!

kma@ABB /usr/bin
$ net start cygserver
The CYGWIN cygserver service is starting.
The CYGWIN cygserver service was started successfully.
~~~
