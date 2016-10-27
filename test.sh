bold=$(tput bold)
normal=$(tput sgr0)

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
        printf "Actual value   = $output\n"
        if [[ $output == *$expected ]]
        then
            printf "√ Correct\n"
        else
            printf "× Wrong\n"
        fi
    else
        ./tester.native $file -e
    fi
    printf "\n"
done
