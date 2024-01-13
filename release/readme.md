# Steps to follow for each release

## Finalise opam code for release
* update version in opam files, configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* run `make tests`, `opam-rt` [checked by github actions]
* update the CHANGE file: take `master_changes.md` content to fill it

## Windows setup

* Download Windows 10 English International 64bit from https://www.microsoft.com/en-gb/software-download/windows10ISO
* Run qemu-img create -f qcow2 Windows-10-x86_64.qcow2 32G
* Run qemu-system-x86_64 -cdrom <path-to-the-windows-iso> -drive file=Windows-10-x86_64.qcow2 -smp 2 -m 6G
* Install Windows. Everything stays as default except:
  * Click "I don’t have a product key"
  * Click "Custom: ..." not "Upgrade: ..."
* This will take a couple of hours
* Upon restart:
  * username: opam
  * no password
  * no location
  * no find my device
  * send required diagnostic data
  * no improve inking & typing
  * no tailored experiences
  * no advertasing
  * no cortana
* Open services.msc and disable the Windows Update service
* Setup OpenSSH Server by following https://learn.microsoft.com/en-us/windows-server/administration/openssh/openssh_install_firstuse?tabs=gui
* Open cmd.exe as administrator and run "notepad C:/ProgramData/ssh/sshd_config"
  * Uncomment and set:
    * PermitEmptyPasswords yes
    * PermitRootLogin yes
    * PasswordAuthentication yes
  * Save and close both applications
* Open regedit.exe and set HKey_Local_Machine\SYSTEM\CurrentControlSet\Control\Lsa\LimitBlankPasswordUse to 0
* Shutdown the computer using the startmenu button

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -am 2.2.0 2.2.0; git push origin 2.2.0)
* /!\ Once the tag pushed, it can be updated [different commit] only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* fetch locally the tag
* generate opam artifacts, using `release/release.sh <tag>` from a macOS/arm64 machine, it requires to have Docker and QEMU installed (see below device requirements)
* generate the signatures using `release/sign.sh <tag>`
* add releases notes (content of `master_changes.md`) in the release draft
* upload everything from `release/out/<tag>`
* finalise the release (publish)

## Publish the release

* add hashes in `install.sh` (and check signatures)
* publish opam packages in opam-repository
* update versions (and messages, if necessary) in https://github.com/ocaml/opam-repository/blob/master/repo

## Announce!

* a blog entry in opam.ocaml.org
* a announcement in discuss.ocaml.org
* copy the blog entry from opam.ocaml.org for https://github.com/ocaml/ocaml.org/tree/main/data/changelog/opam
* announce the release on the OCaml Discord server


## After release

* Bump the version with a `~dev` at the end (e.g. `2.2.0~alpha~dev`)
* Check if reftests needs an update
* Bring the changes to the changelog (CHANGES) from the branch of the release to the `master` branch

### On a release candidate
* create a branch to a `x.y` for rc's and the final release

---

## Device requirements
* Mac M1
* installed: git, gpg, qemu>=8.1.0, docker>=24.0.0
* opam repo with the tag fetched
* Have the secret key available
* Launch docker using the Docker GUI macOS app
