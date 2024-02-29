# Scheme Compiler


### Vagrant

This repository contains a Vagrant setup file called `Vagrantfile` (paraphrasing on `makefile`). Vagrant is a tool for 
sharing VMs. It includes the VM image, setup, configuration, network connections and just about anything you'd want to 
configure before you start working. The vagrantfile contains all the commands required to setup your VM.

#### Using Vagrant
First, download and install [Vagrant](https://www.vagrantup.com/downloads) for your OS. Next, open a terminal in the 
project's root directory (where this `README.md` file is stored) and run `vagrant up`. When running this command
for the first time it might seem like it's stuck on "Downloading" for up to 3 hours, depending on your internet
connection. It's not stuck. 

Note: On windows, Vagrant doesn't give you any indication of download progress, so you can take a look in 
`%HOMEPATH%/. vagrant.d/tmp` and see the file there that represents the downloading VM. Once it reaches 7GB, Vagrant
will continue setting up your VM

The `vagrant up` command will read the Vagrantfile, download the correct VM image, create the VM using VirtualBox, 
create a shared folder between the VM and the host, setup an SSH connection between the VM and start the VM. The first
time you run `vagrant up` the VM image it downloads is rather large (7GB). If you run `vagrant up` again (e.g. if to 
rebuild your VM), Vagrant will use a local cache of the VM image, so it will run MUCH faster (less than a minute).

To connect to your VM, you need to run `vagrant ssh`. This will open an SSH connection to your VM and provide you a 
shell in which you can run your code. Note, the project's folder is mapped to `~/compiler` in the VM (when you log 
in with `vagrant SSH` it will take you to the home folder at `~`), so the first command you execute should be 
`cd compiler`.

If you want to stop your VM, you can execute `vagrant halt`. It will gracefully terminate the VM.

When you want to delete your VM you can execute `vagrant destroy` (helpful if you accidentally destroy you VM somehow). 
Note, if you don't want/can't use `vagrant destroy`, you need to delete the VM from VirtualBox **AND** delete the 
`.vagrant` folder inside the project's root with `rm -rf ./.vagrant` (executed from within the project's root).

### Connecting to the VM graphically
The VM created by `vagrant up` is a regular VM in VirtualBox, so if you'd like, you can just open up the VirtualBox
dashboard, and double-click the relevant VM. The password for the Vagrant user is `vagrant`. 

The VM comes with VScode, Intellij and Emacs all three with their Ocaml plugins. Also it comes preinstalled with Scheme, 
Ocaml, utop, gcc, gdb, nasm and whatever tool we figured you might need. Of course
You can always install any other tool you like from the Ubuntu repositories with `apt install`


### Test Procedure 

#### Test Script
The same testing procedure is implemented in `tests/test_compiler.sh`, except it defines `testfile` using a argument.
To run the `foo` test case described above you should execute:
``` 
  tr -d \r tests/test_compiler.sh
  tests/test_compiler.sh foo
```

Note that first command (the one starting with `tr`). That will fix any cases where a Windows OS has ruined the test
script by changing the end of line from UNIX style (ASCII 10) to Windows style (ASCII 10 followed by ASCII 13). It's not
a part of the test, just a workaround for that destructive behavior for Git on Windows.
