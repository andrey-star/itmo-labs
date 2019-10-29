#!/bin/bash
ps ao pid,command -u andrey | awk '{print $1":"$2}'