Ext.define('example.controller.Main', {
    extend: 'Ext.app.Controller',
    models: ['User'],
    stores: ['Users'],
    views: ['example.view.Main', 
            'example.view.user.List']

});
