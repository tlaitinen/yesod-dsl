Ext.define('example.view.user.List',{
    extend: 'Ext.grid.Panel',
    alias: 'widget.userlist',
    multiSelect: true,

    title: 'Users',
    store: 'Users',
    allowDeselect : true,

    requires: ['Ext.toolbar.Paging','Ext.ux.grid.FiltersFeature',
                'example.store.Users'],

    initComponent: function() {

        this.columns = [
            {header: 'First Name',  dataIndex: 'firstName', flex:1, filterable:true },
            {header: 'Last Name', dataIndex: 'lastName', flex:1, filterable:true }
        ];

        this.bbar = Ext.create('Ext.PagingToolbar', {
            store: Ext.getStore("Users"),
            displayInfo: true,
            displayMsg: 'Users {0} - {1} / {2}',
            emptyMsg: "No users",
            items:[ '-' ]
        });

        this.callParent(arguments);
    },
    features : [{ ftype : 'filters', encode:true}]
 });
