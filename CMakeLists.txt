cmake_minimum_required(VERSION 3.14)

project(ntfs-uefi)

include_directories(/usr/include/efi)
if (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "AMD64" OR ${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
    include_directories(/usr/include/efi/x86_64)
elseif (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "X86")
    include_directories(/usr/include/efi/ia32)
endif()

set(SRC_FILES src/ntfs.c)

add_executable(ntfs ${SRC_FILES})

set_target_properties(ntfs PROPERTIES SUFFIX ".efi")

if(${CMAKE_C_COMPILER_ID} STREQUAL "GNU" OR ${CMAKE_C_COMPILER_ID} STREQUAL "Clang")
    target_compile_options(ntfs PRIVATE "-ffreestanding")
    target_compile_options(ntfs PRIVATE "-fno-stack-protector")
    target_compile_options(ntfs PRIVATE "-fno-stack-check")
    target_compile_options(ntfs PRIVATE "-mno-stack-arg-probe")
    target_compile_options(ntfs PRIVATE "-Werror-implicit-function-declaration")

    target_link_options(ntfs PRIVATE "-nostartfiles")
    target_link_options(ntfs PRIVATE "-shared")

    if(${CMAKE_C_COMPILER_ID} STREQUAL "Clang")
        target_link_options(ntfs PRIVATE "-Wl,--subsystem,efi_boot_service_driver")
    else()
        target_link_options(ntfs PRIVATE "-Wl,--subsystem,11")
    endif()

    if (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "X86")
        target_link_options(ntfs PRIVATE "-e_efi_main")
    else()
        target_link_options(ntfs PRIVATE "-eefi_main")
    endif()
elseif(MSVC)
    target_link_options(ntfs PRIVATE "/SUBSYSTEM:EFI_BOOT_SERVICE_DRIVER")
    target_compile_options(ntfs PRIVATE "/Oi-")
endif()
