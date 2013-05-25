Ext.define('example.store.Comments', {
    extend: 'Ext.data.Store',
    model: 'example.model.Comment',
    pageSize:100,
    remoteFilter:true,
    remoteSort:true,
    proxy: {
        type: 'rest',
        url: 'example/comments',
        reader: {
            type: 'json', 
            root: 'result',
            totalProperty: 'totalCount'
        }
    }
});
