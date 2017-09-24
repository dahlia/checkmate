// Sample JavaScript code
// CHECK global check

function func() {
    // CHECK function-level check
    return null;
}

function otherFunc() {
    // CHECK function-level check 2

    function innerFunc() {
        /*
        CHECK closure check.
        It can be multiline.
        */
        return true;
    }

    return innerFunc;
}
