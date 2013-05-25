Ext.define('example.model.BlogPost', {
    extend: 'Ext.data.Model',
    
    fields: [
        { name: 'id', type: 'int' },
        { name: 'authorId', type: 'int' },
        { name: 'name', type: 'auto' }

    ]
});
