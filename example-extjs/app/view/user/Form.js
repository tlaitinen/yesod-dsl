Ext.define('example.view.user.Form' ,{
    extend: 'Ext.form.Panel',
    alias: 'widget.userForm',

    title: 'Modifying User Information',
    height: 150,
    width: 400,
    bodyPadding: 10,
    buttons: [
        { text : 'OK', name : 'ok' },
        { text : 'Cancel', name : 'cancel'  }
    ],
    items: [
        {
            xtype:'textfield',
            fieldLabel:'First Name',
            name:'firstName',
            allowBlank:false,
            width:350
        },
        {
            xtype:'textfield',
            fieldLabel:'Last Name',
            width:350,
            name:'lastName'
        },
        { 
            xtype:'numberfield',
            fieldLabel:'Age',
            width:350,
            allowBlank:false,
            name:'age'
        }
   ]
});
