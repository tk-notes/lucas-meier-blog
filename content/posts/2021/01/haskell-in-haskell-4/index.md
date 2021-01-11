---
title: "(Haskell in Haskell) 4. Simplification"
date: 2021-01-11T11:04:03+01:00
draft: true
tags:
  - Haskell
  - Programming Languages
  - Compiler
---

# What the simplifier needs to do

# Creating a Stub Simplifier

# The structure of Type Information

## Constructor Information

## Resolving Synonyms

## Using this information

### Resolution Errors

### Contexts

### Resolving Types

### Looking up Constructors

# Gathering Constructors

# Gathering Synonym Information in Theory

## Type Synonyms as a Graph

## Topological Sort

### Depth First Search

# Gathering Synonyms in practice

## Depth First Search in practice

# Finishing Type Information

# Simplifier changes to the AST

### Removing redundant forms

### Single Application

### Builtin functions

### No nested patterns

## Defining our new AST

# Simplifying patterns in theory

## The decision game

## Pattern matrices

## Explaining the basic algorithm

## Named patterns

# Simplifying Patterns in practice

## Basic types

## Basic Utilities

## Core algorithm

# Converting the AST

## Simple Changes

## Definitions

# Gluing things together

## Examples

# Conclusion
