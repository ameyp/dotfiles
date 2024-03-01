# Find: [f]ile by name.
function ff () {
  if ! [[ -n "$1" ]]; then
    echo 'Must specify a search string.'
    return 1
  fi

  find . -name "$1"
}

# Find: name [a]ll.
function fa () { ff "*${1}*" }

# Find: name [s]tarts with.
function fs () { ff "${1}*" }

# Find name [e]nds with.
function fe () { ff "*${1}" }

# Generate an SSL certificate for test use only.
function sslgen-cert-test () {
  if ! [[ -n "$1" ]]; then
    echo 'Must specify name to use.'
    return 1
  fi

  openssl req -new -x509 -nodes -newkey rsa:2048 -keyout $1.key -out $1.pem -days 36524 \
    -subj "/C=US/ST=California/L=San Francisco/O=Example Inc./CN=$1/emailAddress=webmaster@example.com"
}

# Create a new tar archive.
function tarz () {
  if ! [[ -n "$1" ]]; then
    echo 'Must specify directory to use.'
    return 1
  fi

  tar -czf $1.tar.gz $1
}
