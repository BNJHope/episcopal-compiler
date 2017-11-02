BLUE='\033[0;34m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
JASMIN_PATH="./bin/jasmin.jar"

declare -A codesamples
declare -A answers

read -r -d '' codesamples[TestConst] << 'EndOfSample'
episcopal TestConst = 1
EndOfSample
answers[TestConst]=1.0

read -r -d '' codesamples[TestFunc] << 'EndOfSample'
episcopal TestFunc = let sum x y = x + y in 
                         sum 42 42
EndOfSample
answers[TestFunc]=84.0

read -r -d '' codesamples[TestNestedFunc] << EndOfSample
episcopal TestNestedFuncs = let sum x y = x + y in
                            let increment x = sum z 1 in
                            increment 42
EndOfSample
answers[TestNestedFunc]=43.0

read -r -d '' codesamples[TestQuery] << EndOfSample
episcopal TestQuery = sum 42 42 (where
                     query sum x y =
                     x + y)
EndOfSample
answers[TestQuery]=84.0

read -r -d '' codesamples[TestNestedFuncInQuery] << EndOfSample
episcopal TestNestedFuncInQuery = increment 42 (where
                     query increment z =
                     let sum x y = x + y in
                     sum z 1)
EndOfSample
answers[TestNestedFuncInQuery]=43.0

echo -e "${BLUE}--- Building Compiler ---${NC}\n"
cabal clean; cabal build

echo -e "\n${BLUE}--- Compiling ASTs into output_progs ---${NC}\n"
cabal run

echo -e "\n${BLUE}--- Running Jasmin over all output_progs into output_classfiles ---${NC}\n"
EPIS_FILES=output_progs/*
for file in $EPIS_FILES
do
    java -jar $JASMIN_PATH $file -d output_classfiles
done



echo -e "\n${BLUE}--- Testing classfiles ---${NC}"
CLASSFILES=output_classfiles/*
tests_passed=0
total_tests=0
for classfile in $CLASSFILES
do
    filename=$(basename $classfile)
    testname="${filename%%.*}"
    echo -e "\n${GREEN}--- Testing $testname ---${NC}\n"
    echo -e "Equivalent Episcopal Code:\n"
    echo -e "${codesamples[$testname]}"
    expected=${answers[$testname]}
    actual=$(java -cp output_classfiles $testname)
    echo -e "\nExpected Result : $expected"
    echo "Actual Result : $actual"
    if [[ $expected == $actual ]];
    then
        let "tests_passed++"
    else
        echo -e "${RED}$testname FAILED"
    fi
    let "total_tests++"
done

echo -e "\n${BLUE}--- Test Results --- ${NC}\n"
echo -e "${GREEN}Tests Passed : $tests_passed"
echo -e "${GREEN}Total Tests : $total_tests\n"
