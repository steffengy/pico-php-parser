# PicoPHPParser [![Build Status](https://travis-ci.org/steffengy/pico-php-parser.svg?branch=master)](https://travis-ci.org/steffengy/pico-php-parser)

PicoPHPParser is a *lightweight* PHP 7.1 parser, which is (hand-)written in rust.  
It already supports the *most common constructs* of the PHP-language.  

A possible usage for example might be to provide *better error messages* than the original PHP-parser.  
A very early stage of this is shown below:  
![alt text](/screenshot_example.jpg "")

# Testing it

To ensure that the most common constructs of the PHP-language work, we have **2 testing stages**:  

1. **AST tests**:

    > These kind of tests ensure that the information in the AST is correct  
    > and also check the correctness of **meta-information** such as **line numbers**.  
    > There are quite a few of these tests, but they are not as easy to write.  
    
    > These types of tests are run during a simple `cargo test`  
    which is the typical test-command for the **rust ecosystem**.  
2. **Project tests**:  

    > This stage ensures **most common [PHP-projects](#current-test-targets)** parse successfully:  
    > 1. We ensure the tests pass at a clean state of the project.    
    > 2. The parser & pretty printer parse the files & rewrite them   
    > 3. The tests are run a second time to ensure they still work properly.   

## Current Test Targets
- [Laravel (Framework)](https://github.com/laravel/framework)
