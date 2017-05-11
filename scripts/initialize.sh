PROJECT_DIR="$PWD"
SCRIPTS_DIR="$PWD/scripts"

main () {
    prompt_websockets;
    prompt_db;
}
prompt_db () {
    db_help () {
        echo "Enter the Sqlite3 connection string."
        echo "You can enter relative to $PWD or absolute paths"
    }
    while true; do
        read -p "Sqlite3 connection string (h for help): " SALLY_DB_CONN
        case $SALLY_DB_CONN in
            'H' | 'h') db_help ;;
            * ) ;;
        esac
    done
}


function getVars {
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
}

prompt_websockets () {

    warn_appjs () {
        echo "Warning: app.js requires URL for websockets connections"
        echo "Skipping this step may cause problems"
    }
    help_appjs () {
        echo "The javascript file app.js must be modified to point to the correct URL for websockets connections."
    }
    while true; do
        read -p "Where should app.js point to for websockets connections? (h for help)" HOSTNAME
        case $HOSTNAME in
            "") warn; break;;
            'H' | 'h') help_appjs; ;;
            *) {
                $SCRIPTS_DIR/set_websockets_host.sh $HOSTNAME
                echo "Altered app.js to point to $HOSTNAME for websockets"
                break;
               } ;;
        esac
    done
}


main "$@"
