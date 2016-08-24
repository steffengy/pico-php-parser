#!/bin/bash
set -e

# setup a new user (laravel tests fail as root, since file-permissions do not work,
# since root is allowed to do anything)
useradd -ms /bin/bash non_root

# Laravel tests
if [ "$TEST_PROJECT" == "laravel" ]; then
    su -l non_root <<EOSU
    set -e
    git clone https://github.com/laravel/framework --depth=1
    pushd framework
    composer install
    ./vendor/bin/phpunit
    popd
    /ci/tester/target/debug/pico-php-tester parse framework
    pushd framework
    ./vendor/bin/phpunit
    popd
EOSU
fi

# Laravel tests
if [ "$TEST_PROJECT" == "slim" ]; then
    su -l non_root <<EOSU
    set -e
    git clone https://github.com/slimphp/Slim --depth=1
    pushd Slim
    composer install
    ./vendor/bin/phpunit
    popd
    /ci/tester/target/debug/pico-php-tester parse Slim
    pushd Slim
    ./vendor/bin/phpunit
    popd
EOSU
fi
