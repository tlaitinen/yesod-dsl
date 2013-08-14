Ext.define('example.view.Main', {
    extend: 'Ext.container.Container',
    requires:[
        'Ext.tab.Panel',
        'Ext.layout.container.Border',
        'example.view.user.List'
    ],
    
    xtype: 'app-main',

    layout: {
        type: 'border'
    },

    items: [{
        region: 'center',
        xtype: 'tabpanel',
        items:[{
            title: 'Users',
            xtype: 'userlist'
        }]
    }]
});
