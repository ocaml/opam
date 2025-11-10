<#
  .SYNOPSIS
  Downloads and installs opam from binaries distributed on GitHub.

  .DESCRIPTION
  Script is able to perform either a per-machine or per-user installation of
  opam for any version of opam after 2.2.0~beta2. By default it will install the
  latest released version.

  .LINK
  See https://opam.ocaml.org/doc/Install.html for further information.
#>

param (
  # Install the latest alpha, beta or rc
  [switch]$Dev,
  # Install this specific version of opam instead of the latest
  [string]$Version = "2.4.1",
  # Specify the installation directory for the opam binary
  [string]$OpamBinDir = $null
)

$DevVersion = "2.5.0~beta1"
$IsAdmin = (New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
$DefaultBinDir = If ($IsAdmin) {"$Env:ProgramFiles\opam\bin"} Else {"$Env:LOCALAPPDATA\Programs\opam\bin"}

$SHA512s = @{
  "opam-2.2.0-beta2-x86_64-windows.exe"  = "74f034ccc30ef0b2041283ff125be2eab565d4019e79f946b515046c4c290a698266003445f38b91321a9ef931093651f861360906ff06c076c24d18657e2aaf";
  "opam-2.2.0-beta3-x86_64-windows.exe"  = "f09337d94e06cedb379c5bf45a50a79cf2b2e529d7c2bb9b35c8a56d40902ff8c7e3f4de9c75fb5c8dd8272b87b2a2645b14e40ef965376ef0d19afd923acf3b";
  "opam-2.2.0-rc1-x86_64-windows.exe"    = "f2ec830a5706c45cb56a96713e296ef756c3f2904ca15f7c2ad0442916a9585fa1de8070208f2a6bb3a84dc74b677f946f5bc386c8ed1489da802b1d66a5e094";
  "opam-2.2.0-x86_64-windows.exe"        = "171b679c192801f7ec783de4c51d614055cff167d521d31c543b6bed14d9eb3d37a808be4d83ff9acfd0fc1d7008e6582dce245836a51109fc0cb8456528c473";
  "opam-2.2.1-x86_64-windows.exe"        = "7a6dd3012e5553ab85337411d0a145c5d26ceff30687c5e91aaff81d9bd48bbf1852ae37ca8992d57435838a025557512e6ac6958fba7123bacd21d9a8aba31a";
  "opam-2.3.0-alpha1-x86_64-windows.exe" = "2058acf0810c8bbc9d924827484c800b47f74d9c90d70319343a7e8e3ab6592b9f0dea1c76547ea1294b72106c3bcdb91e41718471ec85211ed43d7a8d7372c7";
  "opam-2.3.0-beta1-x86_64-windows.exe"  = "bbac0c1e217525a62a557b038aa738609d16799588d0cec99e89c10bd20fb5f2b61482f4418bd06cf7f7f5c7da7f1585514bd7fdc2ced2bde9673ede58f23905";
  "opam-2.3.0-beta2-x86_64-windows.exe"  = "9d24d5af900c263c4ba0803661f68b21ac85b4ba3281b71edd508c412420f20329cc03958fc1123b1a6adaf15505a9519f70ed4673c2f548dee0319c8a7b2d0b";
  "opam-2.3.0-rc1-x86_64-windows.exe"    = "c40bc8604fa1143b13511bed37296c129b78e275738dfcc945418275d5f0bbd3984d997c5d86b2aea00a5b8026a15751e00111225ccf60979ee23113b98eac8e";
  "opam-2.3.0-x86_64-windows.exe"        = "5b1f46c11a9c46ae6dff02b4076ecdfd6517bc582ae78224e6eb15b921c33f45d2f67d8793b4502c1573bbfd887e3519a4ce5dab13e54ad107dcde1a1204a8d6";
  "opam-2.4.0-alpha1-x86_64-windows.exe" = "f6041831d62cd9f54ed1963dbd52e9eb2d927f2bc49356919d2d7dc7d42824befcdf714ba3b8b1a85bd0fd95b9a863bcedf4c4c90a147f4823fa4a4fad4eec09";
  "opam-2.4.0-alpha2-x86_64-windows.exe" = "bdc30524586527639ad2beeb805a0ed79c94fe70d5f4d68bf908905263f6a4ae755c5d132a7c735b41fa8bb50aac7f0d5300e03f86e8b8c17cbe32b0a7561741";
  "opam-2.4.0-beta1-x86_64-windows.exe"  = "281da611b8db3b40798d94434388c5d12e6f36b8afbdc822ed334f78b5b6f3f33caa4e16a7c15d5f523e77c32a3f6700648f6b35acce8cc1335b5886c471b95c";
  "opam-2.4.0-rc1-x86_64-windows.exe"    = "418f0933d7c53a96f0a53e757d172e4e16df6927f7d668b119894c9775a573c3b3e13f20c4b173eafae633a590e9b71f04c8eca92be108755c310168646c1322";
  "opam-2.4.0-x86_64-windows.exe"        = "e28439bed67fc2b367114f0d5e176690117a0b045066584ca67ec852caaa951354c19892579083122b1c50cd27d323de3b9eb514e54bfd221227190765ede874";
  "opam-2.4.1-x86_64-windows.exe"        = "0b7bed0ea3b009aadb04882568b261c693395f400a79983a42b342b7d78536168dd0d5fab252382b622acbde61be40e3c0b036b974148e78be0edd6c1337d19c";
  "opam-2.5.0-alpha1-x86_64-windows.exe" = "11f1ec33154967a3d625e9d06f1c11700a24b8b7262025e63b124e9a58100e0f75bbc9afbc8b35eaec5c40254b020cea7fec8dcaad98368ee463bab7074ae7f9";
  "opam-2.5.0-beta1-x86_64-windows.exe"  = "e8d2f9f4c644f55399ed5899ec792e2cdb0adbe3f33fadbc3c8ce0496b65287668d1008ab0ae3a163f59e898c053bbd7a8be9730df3f2f15b09f60117e4438f3";
}

Function DownloadAndCheck {
  param (
    [string]$OpamBinUrl,
    [string]$OpamBinTmpLoc,
    [string]$OpamBinName
  )

  if (-not $SHA512s.ContainsKey($OpamBinName)) {
    throw "no sha"
  }

  [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
  (New-Object System.Net.WebClient).DownloadFile($OpamBinUrl, $OpamBinTmpLoc)
  $Hash = (Get-FileHash -Path $OpamBinTmpLoc -Algorithm SHA512).Hash

  if ($Hash -ne "$($SHA512s[$OpamBinName])") {
    throw "Checksum mismatch, a problem occurred during download."
  }
}

if (-not [System.Environment]::Is64BitOperatingSystem) {
  throw "opam requires a 64-bit version of Windows"
}

if ($Dev.IsPresent) {
  $Version = $DevVersion
}

$Tag = $Version.Replace("~", "-")
$Arch = "x86_64"
$OS = "windows"
$OpamBinUrlBase = "https://github.com/ocaml/opam/releases/download/"
$OpamBinName = "opam-$Tag-$Arch-$OS.exe"
$OpamBinUrl = "$OpamBinUrlBase$Tag/$OpamBinName"

$OpamBinTmpLoc = "$Env:TEMP\$OpamBinName"

if (-not (Test-Path -Path $OpamBinTmpLoc -IsValid)) {
  throw "Failed to determine a temporary path for downloading opam"
}

Write-Host "## Downloading opam $Version for Windows on x86_64"
$esc = [char]27

if ([string]::IsNullOrEmpty($OpamBinDir)) {
  $OpamBinDir = Read-Host "## Where should it be installed? [$esc[1m$DefaultBinDir$esc[0m]"
  if ([string]::IsNullOrEmpty($OpamBinDir)) {
    $OpamBinDir = $DefaultBinDir
  }
}

if (-not (Test-Path -Path $OpamBinDir -IsValid) -or $OpamBinDir -contains ';') {
  throw "Destination given for installation is not a valid path"
}

# Check existing opam binaries
$AllOpam = Get-Command -All -Name opam -CommandType Application -ErrorAction Ignore | ForEach-Object -MemberName Source
foreach($OneOpam in $AllOpam) {
  if ($OneOpam -ne "$OpamBinDir\opam.exe") {
    throw "We detected another opam binary installed at '$OneOpam'. To ensure problems won't occur later, please uninstall it or remove it from the PATH"
  }
}

DownloadAndCheck -OpamBinUrl $OpamBinUrl -OpamBinTmpLoc $OpamBinTmpLoc -OpamBinName $OpamBinName

# Install the binary
if (-not (Test-Path -Path $OpamBinDir -PathType Container)) {
  [void](New-Item -Force -Path $OpamBinDir -Type Directory)
}
[void](Move-Item -Force -Path $OpamBinTmpLoc -Destination "$OpamBinDir\opam.exe")

# Add the newly installed binary to PATH for this and future sessions
$EnvTarget = If ($IsAdmin) {'MACHINE'} Else {'USER'}
foreach ($loc in $EnvTarget, 'PROCESS') {
  $PATH = [Environment]::GetEnvironmentVariable('PATH', $loc)
  if (-not ($PATH -split ';' -contains $OpamBinDir)) {
    [Environment]::SetEnvironmentVariable('PATH', "$OpamBinDir;$PATH", $loc)
    if ($loc -ne 'PROCESS') {
      Write-Host "## opam has added $OpamBinDir to the $loc PATH for future sessions"
      Write-Host ""
      Write-Host "Please use the Environment Variables editor in Control Panel to remove"
      Write-Host "this entry if you subsequently decide to remove opam from your system"
      Write-Host "(it can be started by running $esc[1mrundll32 sysdm.cpl,EditEnvironmentVariables$esc[0m)"
    }
  }
}

Write-Host "## opam $Version installed to $OpamBinDir"
