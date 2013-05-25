Ext.define('example.model.Comment', {
    extend: 'Ext.data.Model',
    
    fields: [
        { name: 'id', type: 'int' },
        { name: 'blogPostId', type: 'int' },
        { name: 'authorId', type: 'int' },
        { name: 'comment', type: 'auto' },
        { name: 'time', type: 'auto' },
        { name: 'commentState', type: 'auto' }

    ]
});
