use inc::Module::Install;

name     'Test-Script-Run';
all_from 'lib/Test/Script/Run.pm';
author   'sunnavy <sunnavy@bestpractical.com>';
license  'perl';

test_requires 'Test::More';
requires 'IPC::Run3';
requires 'Test::Exception';

auto_install;

WriteAll;

