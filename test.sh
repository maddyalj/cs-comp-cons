bold=$(tput bold)
normal=$(tput sgr0)

echo ""
for file in tests/*
do
    printf "${bold}## Testing $(basename $file)${normal}\n"
    ./tester.native $file
    echo ""
done
