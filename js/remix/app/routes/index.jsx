function Index() {
    return <button onClick={(function () {    alert('hi!');    __PS_MV_REG = [];    return null;}).bind(this)}>{1 + 1}</button>;
};
module.exports = Index;