* From the "release" directory (where this here readme.md file should be)...
* Download Windows 10 English International 64bit from https://www.microsoft.com/en-gb/software-download/windows10ISO
* Run qemu-img create -f qcow2 Windows-10-x86_64.qcow2 32G
* Run qemu-system-x86_64 -cdrom Win10_22H2_EnglishInternational_x64v1.iso -drive file=Windows-10-x86_64.qcow2 -smp 8 -m 6G -net nic -net user,restrict=on -machine q35
* Install Windows. Everything stays as default except:
  * Click "I don’t have a product key"
  * Choose "Windows 10 Pro"
  * Click "Custom: ..." not "Upgrade: ..."
* This will take an hour or so
* Select the default keyboard then wait some more
* Upon restart:
  * no online account
  * yes, i want the limited experience
  * username: opam
  * password: password
  * 3 hints: just random or something
  * no location
  * no find my device
  * no improve inking & typing
  * send required diagnostic data
  * no advertising ID
  * no cortana
* Open gpedit.msc and right-click on "Computer Configuration -> Administrative Templates -> Windows Components -> Windows Update -> Configure Automatic Updates", click Edit and in the new window click on Enabled and then select "2. Notify for download and auto install", click Apply and then Ok.
* Start -> Power off
* Run qemu-system-x86_64 -drive file=Windows-10-x86_64.qcow2 -smp 8 -m 6G -machine q35
* Answer yes to the "would you like to be discoverable on the network" prompt
* Open the Settings app
  * "System -> Power & Sleep" and select "Never" on the "When plugged in, turn off after" drop-down menu
  * Put the slider on "Best performance"
  * search for "Add an optional feature", then search for "OpenSSH" and install "OpenSSH Server"
* Run services.msc
  * Double-click OpenSSH SSH Server then select Automatic from the Startup drop-down menu
  * Click Ok
* Shutdown the computer using the startmenu button
