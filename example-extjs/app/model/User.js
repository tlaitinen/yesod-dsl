Ext.define('example.model.User', {
    extend: 'Ext.data.Model',
    
    fields: [
        { name: 'id', type: 'int' },
        { name: 'firstName', type:'string'},
        { name: 'lastName', type:'string'},
        { name: 'age', type:'auto'}
    ]
});
