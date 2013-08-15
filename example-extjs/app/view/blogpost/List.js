Ext.define('example.view.blogpost.List',{
    extend: 'Ext.grid.Panel',
    alias: 'widget.blogpostlist',
    multiSelect: true,

    title: 'Blog Posts',
    store: 'BlogPosts',
    allowDeselect : true,

    requires: ['Ext.toolbar.Paging',
                'example.store.BlogPosts'],

    initComponent: function() {

        this.columns = [
            {header: 'Title',  dataIndex: 'name', flex:3},
            {header:'Published', dataIndex:'time', flex:1},
            {header: 'Author first name', dataIndex: 'authorFirstName' },
            {header: 'Author last name', dataIndex:'authorLastName'},
        ];

        this.bbar = Ext.create('Ext.PagingToolbar', {
            store: Ext.getStore("BlogPosts"),
            displayInfo: true,
            displayMsg: 'Blog posts {0} - {1} / {2}',
            emptyMsg: "No blog posts",
            items:[ '-' ]
        });

        this.callParent(arguments);
    },
    dockedItems: [
        {
            xtype: 'toolbar',
            dock: 'top',
            ui:'footer',
            items: [
                { xtype: 'textfield', itemId:'textSearch', flex:1,
                emptyText: 'Search'}
          ]
        }
    ]
 });
