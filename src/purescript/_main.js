const Main = require("../../output/Main")

function main() {
    console.log("Running app");
    Main.main();
}

console.log('Starting app');

// main();  //  calling `main` synchronously does not work for Concur widgets, as
//  it will try to render them before the dom is fully layed out.
window.onload = main;
