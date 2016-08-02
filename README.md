# PicoPHPParser [![Build Status](https://travis-ci.org/steffengy/pico-php-parser.svg?branch=master)](https://travis-ci.org/steffengy/pico-php-parser)

PicoPHPParser is a *lightweight* PHP 7.1 parser, which was (hand-)written in rust.  
It already supports the *most common constructs* of the PHP-language.  

A possible usage for example might be to provide *better error messages* than the original PHP-parser.  
A very early stage of this is shown below:  
![alt text](/screenshot_example.jpg "")

# Testing it

To ensure that the most common constructs of the PHP-language work, we have **2 testing stages**:  

1. **AST tests**:

    > These kind of tests ensure that the information in the AST is correct  
    > and also check the correctness of **meta-information** such as **line numbers**.  
    > There are quite a few, but they are not as easy to write.  
    > A simple `cargo test`, as typical for the **rust ecosystem**, should suffice.  
2. **Project tests**:  

    > The goal is to test against the **most common [PHP-projects](#current-test-targets)**  
    > to ensure they can be **parsed & pretty printed**. To verify this works, we run the **tests before  
    > and after**, to basically ensure the parser aswell as the "pretty" printer work.  

# Current Test Targets
- [Laravel (Framework)](https://github.com/laravel/framework)
