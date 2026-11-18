distributed systems lab
=======================

Using gradle
------------

### Generate local test fixtures

Run this once from this directory before building, testing, or starting the
application:

    ./generate-test-fixtures.sh

The script requires OpenSSL and creates disposable RSA and HMAC keys for the
local lab. The generated directories are ignored by Git. The script does not
overwrite existing key directories. Do not use these fixtures in production.

### Compile & Test

Gradle is the build tool we are using. Here are some instructions:

Compile the project using the gradle wrapper:

    ./gradlew assemble

Compile and run the tests:

    ./gradlew build

### Run the applications

The gradle config config contains several tasks that start application components for you.
You can list them with

    ./gradlew tasks --all

And search for 'Other tasks' starting with `run-`. For example, to run the monitoring server, execute:
(the `--console=plain` flag disables CLI features, like color output, that may break the console output when running a
interactive application)

    ./gradlew --console=plain run-monitoring
