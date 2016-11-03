bold=$(tput bold)
normal=$(tput sgr0)
wrong_results=0

printf "\n"
for file in tests/*
do
    printf "${bold}### Testing $(basename $file)${normal}\n"
    if [[ $file == *.e_*.* ]]
    then
        expected=${file#*.e_}
        expected=${expected%.*}
        printf "# Expected value = %d\n" $expected
        output=$(./tester.native $file -e)
        printf "# Actual value = $output"
        if [[ $output == *$expected ]]
        then
            printf " √ Correct\n"
        else
            ((wrong_results++))
            printf " × FAILED\n"
        fi
        optimized=$(./tester.native $file -e -o)
        printf "# (With optimization) = $optimized"
        if [[ $optimized == *$expected ]]
        then
            printf " √ Correct\n"
        else
            ((wrong_results++))
            printf " × FAILED\n"
        fi
    else
        ./tester.native $file -e
    fi
    printf "\n"
done

if [[ $wrong_results = 0 ]]
then
    echo '√√√ All tests passed!'
else
    echo '×' $wrong_results 'TESTS FAILED!'
fi
