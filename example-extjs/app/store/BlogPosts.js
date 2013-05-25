Ext.define('example.store.BlogPosts', {
    extend: 'Ext.data.Store',
    model: 'example.model.BlogPost',
    pageSize:100,
    remoteFilter:true,
    remoteSort:true,
    proxy: {
        type: 'rest',
        url: 'example/blogposts',
        reader: {
            type: 'json', 
            root: 'result',
            totalProperty: 'totalCount'
        }
    }
});
