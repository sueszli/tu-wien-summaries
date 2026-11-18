#!/bin/sh
set -eu

CDPATH='' cd "$(dirname "$0")"
test_keys=src/test/resources/keys
if [ -e keys ] || [ -L keys ] || [ -e "$test_keys" ] || [ -L "$test_keys" ]; then
    printf '%s\n' 'A key directory already exists. No fixtures were changed.' >&2
    exit 1
fi
if ! command -v openssl >/dev/null 2>&1; then
    printf '%s\n' 'OpenSSL is required to generate local test fixtures.' >&2
    exit 1
fi

umask 077
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mailservice-fixtures.XXXXXXXX")
trap 'rm -rf -- "$scratch"' 0
trap 'exit 1' HUP INT TERM

mkdir -p "$scratch/run/client" "$scratch/run/server" "$scratch/test/client" "$scratch/test/server"
for server in mailbox-earth-planet mailbox-univer-ze transfer-1 transfer-2; do
    if ! openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out "$scratch/pair.pem" 2>"$scratch/openssl.log"; then
        cat "$scratch/openssl.log" >&2
        exit 1
    fi
    openssl pkcs8 -topk8 -nocrypt -outform DER -in "$scratch/pair.pem" -out "$scratch/run/server/$server.der"
    openssl pkey -in "$scratch/pair.pem" -pubout -outform DER -out "$scratch/run/client/${server}_pub.der"
done

generate_hmac() {
    # The coursework loader reads exactly 1024 ASCII hex characters.
    key=$(openssl rand -hex 512)
    [ "${#key}" -eq 1024 ]
    printf '%s' "$key" > "$1"
}
generate_hmac "$scratch/run/hmac.key"
generate_hmac "$scratch/run/wrongHmac.key"

cp "$scratch/run/client/mailbox-earth-planet_pub.der" "$scratch/test/client/"
cp "$scratch/run/server/mailbox-earth-planet.der" "$scratch/test/server/"

# Reserve both paths without overwriting material from another setup.
mkdir keys
if ! mkdir "$test_keys"; then
    rmdir keys
    exit 1
fi
mv "$scratch/run/"* keys/
mv "$scratch/test/"* "$test_keys/"
printf '%s\n' 'Generated local test fixtures. Do not use them outside this coursework.'
