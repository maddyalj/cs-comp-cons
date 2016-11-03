bold=$(tput bold)
normal=$(tput sgr0)
wrong_results=0

printf "\n"
for file in tests/*
do
    printf "${bold}## Testing $(basename $file)${normal}\n"
    if [[ $file == *.e_*.* ]]
    then
        expected=${file#*.e_}
        expected=${expected%.*}
        printf "Expected value = %d\n" $expected
        output=$(./tester.native $file -e)
        printf "Actual value = $output\n"
        if [[ $output == *$expected ]]
        then
            printf "√ Correct\n"
        else
            ((wrong_results++))
            printf "× Wrong\n"
        fi
        optimized=$(./tester.native $file -e -o)
        printf "(With optimization) = $optimized\n"
        if [[ $optimized == *$expected ]]
        then
            printf "√ Correct\n"
        else
            ((wrong_results++))
            printf "× Wrong\n"
        fi
    else
        ./tester.native $file -e
    fi
    printf "\n"
done

if [[ $wrong_results = 0 ]]
then
    echo '√√√ ALL TESTS PASSED!'
else
    echo '×' $wrong_results 'TESTS FAILED!'
fi
