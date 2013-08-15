Ext.define('example.controller.Main', {
    extend: 'Ext.app.Controller',
    models: ['User','BlogPost'],
    stores: ['Users','BlogPosts'],
    views: ['example.view.Main',
            'example.view.blogpost.List', 
            'example.view.user.List',
            'example.view.user.Form'],
    onLogin: function() {
        Ext.getStore('Users').load();
        Ext.getStore('BlogPosts').load();
    },
    openUserEditor: function() {
        if (Ext.get('userForm') == undefined) {
            var win = new Ext.Window({
                id: "userForm",
                width:420,
                height:190,
                resizable:false,
                items : [{xtype: 'userForm'}]
            });
            win.show();
            return win;
        } else
            return null;
    },

    init: function () {
        Ext.Ajax.request({
            url: 'backend/',
            method: 'get',
            success: function (response) {
                if (response.responseText == 'null') {
                    Ext.getStore('Users').load();
                }
            },
            failure: function (response) {
            }
        });         


        var controller = this;
        this.control({
        'userForm button[name=ok]': {
            click: function(button) {
                var win = button.up('window');
                var form = button.up('form').getForm();
                if (form.isValid()) {
                    record = form.getRecord();
                    form.updateRecord(record);
                    win.close();
                    var store = Ext.getStore('Users');
                    if (!record.getId()) {
                        store.add(record);
                    }
                    store.sync({ callback: function() { store.load(); }});
                } else {
                    Ext.Msg.alert('Validation failed', 'Form validation failed.');
                } 
        }
        }, 
        'userForm button[name=cancel]': {
            click: function(button) {
                var win = button.up('window');
                win.close();
            }
        },
        'userlist': {
                    celldblclick: function(grid, td, cellIndex, record, tr, rowIndex, e, eOpts ) {
                     var win = controller.openUserEditor();
                     if (win) {
                        var form = Ext.ComponentQuery.query('window userForm')[0];
                        form.loadRecord(record);
                      }
                  }
           },

            "button[name=logout]" : {
                click: function() {
                    Ext.Ajax.request({
                        url: 'backend/auth/logout',
                        method: 'post',
                        success: function (response) {
                            Ext.getStore('Users').removeAll();
                        },
                        failure: function (response) {
                        }
                    });         

                }
            },
            "button[name=login]": {
                    click: function() {
                        var win = window.open('backend/auth/login','_blank', 'width=640,height=480');
                        var timer = setInterval(checkChild, 500);

                        function checkChild() {
                            if (win.closed) {
                                controller.onLogin();
                                clearInterval(timer);
                            }
                        }
                    }
                }
            });
    }
});
