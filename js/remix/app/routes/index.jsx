function Index() {
    return <button onClick={(function () {    alert('Hello!!!');    __PS_MV_REG = [];    return null();}).bind(this)}>{'Click ' + 'me! again!'}</button>;
};
module.exports = Index;