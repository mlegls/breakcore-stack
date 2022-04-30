import { Button, Alert, Space } from 'antd';
function Index() {
    return <div className="bg-black w-full p-2"><Space direction="vertical"><Alert message="Hello from the Remix Breakcore Stack!" type="success"></Alert><Button onClick={(function () {
    alert('It works.');
    __PS_MV_REG = [];
    return null();
}).bind(this)} type="primary">Click Me :)</Button></Space></div>;
};
module.exports = Index;