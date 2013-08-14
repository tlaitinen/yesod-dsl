Ext.define('example.store.Users', {
    extend: 'Ext.data.Store',
    model: 'example.model.User',
    pageSize:100,
    remoteFilter:true,
    remoteSort:true,
    proxy: {
        type: 'rest',
        url: 'backend/example/users',
        reader: {
            type: 'json', 
            root: 'result',
            totalProperty: 'totalCount'
        }
    }
});
