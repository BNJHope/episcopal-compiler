BLUE='\033[0;34m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
JASMIN_PATH="./lib/jasmin.jar"

# If the output_progs directory
# does not exist then create it.
if [ ! -d output_progs ]; then
    mkdir output_progs
fi

# If the output_classfiles directory
# does not exist then create it.
if [ ! -d output_classfiles ]; then
    mkdir output_classfiles
fi

# Create associative arrays
# that map the test names
# to equivalent code samples
# and expected outputs.
declare -A codesamples
declare -A answers

# Test for basic constant.
read -r -d '' codesamples[TestConst] << 'EndOfSample'
episcopal TestConst = 1
EndOfSample
answers[TestConst]=1.0

# Test for basic function usage.
read -r -d '' codesamples[TestFunc] << 'EndOfSample'
episcopal TestFunc = let sum x y = x + y in 
                         sum 42 42
EndOfSample
answers[TestFunc]=84.0

# Test for nested function.
read -r -d '' codesamples[TestNestedFunc] << EndOfSample
episcopal TestNestedFuncs = let sum x y = x + y in
                            let increment x = sum z 1 in
                            increment 42
EndOfSample
answers[TestNestedFunc]=43.0

# Test for creating and calling a query.
read -r -d '' codesamples[TestQuery] << EndOfSample
episcopal TestQuery = sum 42 42 (where
                     query sum x y =
                     x + y)
EndOfSample
answers[TestQuery]=84.0

# Test calling a query with a nested function.
read -r -d '' codesamples[TestNestedFuncInQuery] << EndOfSample
episcopal TestNestedFuncInQuery = increment 42 (where
                     query increment z =
                     let sum x y = x + y in
                     sum z 1)
EndOfSample
answers[TestNestedFuncInQuery]=43.0

# Test getting an instance of a Bernoulli object.
read -r -d '' codesamples[TestBernoulli] << EndOfSample
episcopal TestNestedFuncInQuery = Bernoulli 0.5 
EndOfSample
answers[TestBernoulli]="Bernoulli p:0.5"

read -r -d '' codesamples[TestSampleBernoulli] << EndOfSample
episcopal TestSampleBernoulli = sample Bernoulli 0.5 
EndOfSample
answers[TestSampleBernoulli]=0.0

read -r -d '' codesamples[TestBeta] << EndOfSample
episcopal TestBeta = Beta 5 12
EndOfSample
answers[TestBeta]="Beta Alpha: 12.0 Beta: 5.0"

read -r -d '' codesamples[TestSampleBeta] << EndOfSample
episcopal TestSampleBeta = sample Beta 5 12
EndOfSample
answers[TestSampleBeta]="Float value"

# Test getting an instance of a Bernoulli object.
read -r -d '' codesamples[TestFlip] << EndOfSample
episcopal TestFlip = Flip 0.5 
EndOfSample
answers[TestFlip]="Flip p:0.5"

read -r -d '' codesamples[TestSampleFlip] << EndOfSample
episcopal TestSampleFlip = sample Flip 0.5 
EndOfSample
answers[TestSampleFlip]=0.0

read -r -d '' codesamples[TestNormal] << EndOfSample
episcopal TestNormal = Normal 5 0.5
EndOfSample
answers[TestNormal]="Normal Mean: 5.0 Standard Deviation: 0.5"

read -r -d '' codesamples[TestSampleNormal] << EndOfSample
episcopal TestSampleNormal = sample Normal 5 0.5
EndOfSample
answers[TestSampleNormal]="Float value"

read -r -d '' codesamples[TestSubtract] << EndOfSample
episcopal TestSubtract = 42 - 5
EndOfSample
answers[TestSubtract]="37.0"

read -r -d '' codesamples[TestMultiply] << EndOfSample
episcopal TestMultiply = 42 * 5
EndOfSample
answers[TestMultiply]="210.0"

read -r -d '' codesamples[TestOrTrueTrue] << EndOfSample
episcopal TestOrTrueTrue = True or True
EndOfSample
answers[TestOrTrueTrue]="1.0"

read -r -d '' codesamples[TestOrTrueFalse] << EndOfSample
episcopal TestOrTrueFalse = True or False
EndOfSample
answers[TestOrTrueFalse]="1.0"

read -r -d '' codesamples[TestOrFalseFalse] << EndOfSample
episcopal TestOrFalseFalse = False or False
EndOfSample
answers[TestOrFalseFalse]="0.0"

read -r -d '' codesamples[TestAndTrueTrue] << EndOfSample
episcopal TestAndTrueTrue = True and True
EndOfSample
answers[TestAndTrueTrue]="1.0"

read -r -d '' codesamples[TestAndTrueFalse] << EndOfSample
episcopal TestAndTrueFalse = True and False
EndOfSample
answers[TestAndTrueFalse]="0.0"

read -r -d '' codesamples[TestAndFalseFalse] << EndOfSample
episcopal TestAndFalseFalse = False and False
EndOfSample
answers[TestAndFalseFalse]="0.0"

read -r -d '' codesamples[Test42GreaterThan5] << EndOfSample
episcopal Test42GreaterThan5 = 42 > 5
EndOfSample
answers[Test42GreaterThan5]="1.0"

read -r -d '' codesamples[Test42LessThan5] << EndOfSample
episcopal Test42LessThan5 = 42 < 5
EndOfSample
answers[Test42LessThan5]="-1.0"

read -r -d '' codesamples[Test5GreaterThan42] << EndOfSample
episcopal Test5GreaterThan42 = 5 > 42
EndOfSample
answers[Test5GreaterThan42]="-1.0"

read -r -d '' codesamples[Test5LessThan42] << EndOfSample
episcopal Test5LessThan42 = 5 < 42
EndOfSample
answers[Test5LessThan42]="1.0"

read -r -d '' codesamples[Test5Equals42] << EndOfSample
episcopal Test5Equals42 = 5 = 42
EndOfSample
answers[Test5Equals42]="-1.0"

read -r -d '' codesamples[Test5Equals5] << EndOfSample
episcopal Test5Equals5 = 5 = 5
EndOfSample
answers[Test5Equals5]="0.0"

read -r -d '' codesamples[TestObservationTrue] << EndOfSample
episcopal TestObservationTrue = observe True in
                                42
EndOfSample
answers[TestObservationTrue]="Valid with 42 as value"

read -r -d '' codesamples[TestObservationFalse] << EndOfSample
episcopal TestObservationFalse = observe False in
                                42
EndOfSample
answers[TestObservationFalse]="Invalid with value 0"

# Build the compiler.
echo -e "${BLUE}--- Building Compiler ---${NC}\n"
rm output_progs/* output_classfiles/*
cabal clean; cabal build

# Run the compiler to compile the ASTs.
echo -e "\n${BLUE}--- Compiling ASTs into output_progs ---${NC}\n"
cabal run

# Run Jasmin over all of the output files.
echo -e "\n${BLUE}--- Running Jasmin over all output_progs into output_classfiles ---${NC}\n"
EPIS_FILES=output_progs/*
for file in $EPIS_FILES
do
    java -jar $JASMIN_PATH $file -d output_classfiles
done

# Run tests over all of the classfiles.
echo -e "\n${BLUE}--- Testing classfiles ---${NC}"
# javac -cp  -d output_classfiles lib/Distribution/*.java
javac lib/Distribution/*.java -cp lib/commons-math3-3.6.1.jar -d output_classfiles
CLASSFILES=output_classfiles/Test*
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
    actual=$(java -cp output_classfiles:"lib/commons-math3-3.6.1.jar" $testname)
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

 # Output the total results of the test.
 echo -e "\n${BLUE}--- Test Results --- ${NC}\n"
 echo -e "${GREEN}Tests Passed : $tests_passed"
 echo -e "${GREEN}Total Tests : $total_tests\n"
