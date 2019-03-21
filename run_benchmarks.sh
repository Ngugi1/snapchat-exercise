for i in {1..64}
do
    echo "---"
    echo "> fib, $i threads"
    erl +S $i -noshell -s benchmark test_fib -s init stop > output-fib-$i.txt
    echo "---"
    echo "> homepage, $i threads"
    erl +S $i -noshell -s benchmark test_homepage -s init stop > output-homepage-$i.txt
    echo "---"
    echo "> make_story, $i threads"
    erl +S $i -noshell -s benchmark test_make_story -s init stop > output-make_story-$i.txt
done
