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

    items: [ {
        region:'west',
        title:'Info',
        xtype:'panel',
        width:150,
        layout:'vbox',
        items: [ { 
            xtype: 'box',
            width: '100%',
            html:'This is a simple ExtJS application demonstrating '
                + ' basic usage of yesod-dsl with ExtJS. '
                + ' Some of the handlers require you to login while some are public.'},
        { xtype:'panel', layout:'hbox', items: [
                { xtype:'button', name:'login', text:'Login'},
                { xtype:'button', name:'logout', text:'Logout'} ]
            }]
        },
        {
        region: 'center',
        xtype: 'tabpanel',
        items:[{
            title: 'Users',
            xtype: 'userlist'
        }, {
            title: 'Blog posts',
            xtype: 'blogpostlist'
        }
        ]}]
});
