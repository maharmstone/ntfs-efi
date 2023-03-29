ntfs-efi
---------

btrfs-efi is an NTFS filesystem driver for EFI. It is intended for use with the free
Windows bootloader [Quibble](https://github.com/maharmstone/quibble), but you
should be able to use it for anything EFI-related.

Thanks to [Eric Biggers](https://github.com/ebiggers), who [successfully reverse-engineered](https://github.com/ebiggers/ntfs-3g-system-compression/) Windows 10's
"WOF compressed data", and whose code I've used here.

Changelog
---------

* 20230328
  * Initial release

To do
-----

* LZX WOF compression
* LZNT1 compression
* Hide special files in root
* Free space, volume label, etc.
* Symlinks
* Case-sensitive directories
