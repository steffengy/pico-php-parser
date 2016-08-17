#!/bin/bash
set -e

# setup a new user (laravel tests fail as root, since file-permissions do not work,
# since root is allowed to do anything)
useradd -ms /bin/bash non_root

# Laravel tests
if [ "$TEST_PROJECT" == "laravel" ]; then
    su -l non_root <<EOSU
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

# YII2 tests
if [ "$TEST_PROJECT" == "yii2" ]; then
    su -l non_root <<EOSU
    git clone https://github.com/yiisoft/yii2 --depth=1
    pushd yii2
    composer install
    ./vendor/bin/phpunit --exclude-group mssql,oci,wincache,xcache,zenddata,cubrid
    popd
    /ci/tester/target/debug/pico-php-tester parse yii2
    pushd yii2
    ./vendor/bin/phpunit --exclude-group mssql,oci,wincache,xcache,zenddata,cubrid
    popd
EOSU
fi
