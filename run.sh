#!/bin/sh
HELP_TEXT="Valid program names are:\n - hello_world\n - test"

if [ $# -eq 0 ]
  then echo "Usage: $0 <program>\n$HELP_TEXT"
else
  if [ $1 = "hello_world" ]
    then ./main "$(cat ./examples/hello_world.txt)"
  elif [ $1 = "test" ]
    then ./main "$(cat ./examples/test.txt)"
  else
    echo "Program '$1' couldn't be found. $HELP_TEXT"
  fi
fi
