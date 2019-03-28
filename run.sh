#!/bin/sh
# module function  args = NumberOfFriends ,NumberOfStories Times
erl -noshell +S 6:6 -run $1 $2 $3 $4 $5 $6 -s init stop
