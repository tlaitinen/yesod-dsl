Ext.define('example.model.User', {
    extend: 'Ext.data.Model',
    
    fields: [
        { name: 'id', type: 'int' },
        { name: 'name', type: 'auto' }
    ]
});
