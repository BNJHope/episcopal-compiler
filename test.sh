BLUE='\033[0;34m'
NC='\033[0m'
echo -e "${BLUE}--- BUILDING COMPILER ---${NC}\n"
cabal clean; cabal build
echo -e "\n${BLUE}--- COMPILING ASTS INTO output_progs ---${NC}\n"
./dist/build/pldi-p1/pldi-p1
