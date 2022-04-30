import { Button, Alert } from 'antd';
function Index() {
    return <div><Button onClick={(function () {
    alert('Hello!!!');
    __PS_MV_REG = [];
    return null();
}).bind(this)} type="primary">Click Me!</Button><Alert message="Hello from the Remix Breakcore Stack!" type="success"></Alert></div>;
};
module.exports = Index;