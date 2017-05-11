
vars=(SALLY_DB SALLY_USE_WS SALLY_PORT)

echo "Enter the parameters (full paths prefered)"

for var in ${vars[*]}
do
    echo -n $var:
    read $var
done

SALLY_DR=$(dirname $SALLY_DB)

echo $SALLY_DR



for var in ${vars[*]}
do
    echo $var is ${!var}
done
