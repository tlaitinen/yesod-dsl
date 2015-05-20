// This example demonstrates how to generate ExtJS models, stores, grids, comboboxes and forms from yesod-dsl JSON output
// requires jQuery and underscore
$(function() {
    // url to yesod-dsl JSON output
    $.get("resources/services.json").done(function(defs) {

        // fill in user interface texts here or customize translate-function below
        var strings = {
            'Search' : 'Search',
            'usergroupscombo.emptyText' : 'Filter by user group',
            'userlist.title' : 'Users',
            'userlist.paging.displayMsg' : 'Users {0} - {1} / {2}',
            'userlist.paging.emptyMsg' : 'No users',
            'grouplist.title' : 'userlist.title',
            'grouplist.paging.displayMsg' : 'userlist.paging.displayMsg',
            'grouplist.paging.emptyMsg' : 'userslist.paging.emptyMsg'
        };

        // customize generated ExtJS classes
        var config = {
            name: 'MyApp',
            urlBase: 'services/data',
            defaultTextFieldPlugins: ['clearbutton'], // http://www.eekboom.de/ClearButton.html
            defaultPageSize: 100,
            defaultStoreFilters: [
                {
                    field:'hideDeleted',
                    value:"true"
                }
            ],
            routes: {
                users : {
                    grids : [
                        {
                            widget: 'userlist',
                            fields: ['name'],
                            preload:true,
                            toolbar: [ 
                                {
                                    xtype:'usergroupscombo',
                                    filterField: 'userGroupId'
                                } 
                            ]
                        },
                        {
                            // user grid without user group filter
                            widget: 'groupuserlist',
                            fields: ['name'],
                            searchField: false
                        }
                     ]
                }
            }
        };
        function createStore(storeCls) {
            var store = Ext.create(storeCls);
            config.defaultStoreFilters.forEach(function(cf) {
                store.addFilter(
                    new Ext.util.Filter({
                        id: cf.field,
                        property: cf.field,
                        value: cf.value,
                    }), false
                );
            });
            return store;
        }

        // customized ExtJS ComboBox to fetch a record by ID field (call configStore when value set)
        Ext.define(config.name + '.view.Combo',{
            extend: 'Ext.form.field.ComboBox',
            minChars : 1,
            typeAhead: true,
            store:'ext-empty-store',
            queryMode: 'remote',
            pageSize:10,
            valueField: 'id',
            displayField: 'name',
            plugins: config.defaultTextFieldPlugins || [],
            initComponent: function() {
                this.callParent(arguments);
                this.store = createStore(this.myStore);
            },
            configStore: function(extraFilters) {
                var value = this.getValue()
                this.store.addFilter(
                    new Ext.util.Filter({
                        id: 'id',
                        property: 'id',
                        value: ''+value
                    }), false
                );
                var combo = this;
                (function(c,v) {
                   c.store.load(function() {
                        c.setValue(v);
                        c.store.filters.removeAtKey('id');

                        if (c.getFilters) {
                            var filters = c.getFilters();
                            for (var i = 0; i < filters.length; i++) {
                                c.store.addFilter(filters[i], false);
                            }
                            if (extraFilters) {
                                for (var i = 0; i < extraFilters.length; i++) {
                                    c.store.addFilter(extraFilters[i], false);
                                }
                            }

                        }
                    });
                })(combo,value);
            }
        });


        function endsWith(str, suffix) {
            return str.indexOf(suffix, str.length - suffix.length) !== -1;
        }
        function translate(k,d) {
            if (k in strings) {
                while (k in strings) {
                    k = strings[k];
                }
                return k;
            } else if (d) {
                return d;
            } else {
                if (endsWith(k, ".search"))
                    return strings["Search"];
                return k;
            }
        };

        function idTooltip(record) { 
            return "ID " + record.get('id');
        }

        defs.routes.forEach(function (r) {
            r.handlers.forEach(function (h) {
               
                var name = '', url = config.urlBase;
                r.path.forEach(function (pp) {
                    url += "/";
                    if (pp.type == "string") {
                        name += pp.value;
                        url += pp.value
                    } else {
                        name += pp.references;
                        url += '(ID)';
                    }
                }); 
                var modelName = config.name + '.model.' + name,
                    storeName = config.name + '.store.' + name,
                    comboName = config.name + '.view.' + name + '.Combo',
                    routeCfg = config.routes[name] || {};

                // create models and stores for GET handlers without parameters  
                if (h.type == "GET" && r.path.length == 1 && r.path[0].type == "string") {

                         
                    Ext.define(modelName, {
                        extend: 'Ext.data.Model',
                        fields: h.outputs
                    }, function (model) {
                        Ext.define(storeName, {
                            extend: 'Ext.data.Store',
                            model: modelName,
                            pageSize: config.defaultPageSize,
                            remoteFilter: true,
                            remoteSort: true,
                            proxy: {
                                type: 'rest',
                                url: url,
                                reader: {
                                    type: 'json',
                                    root: 'result',
                                    totalProperty: 'totalCount'
                                }
                            }
                        }, function(storeClass) {
                            var store = createStore(storeClass);

                            // ComboBox for entities with 'name' field
                            if (_.find(h.outputs, function (o) { return o.name == 'name'; })) {
                                Ext.define(comboName, {
                                    extend: config.name + '.view.Combo',
                                    alias: 'widget.' + name + 'combo',
                                    emptyText: translate(name + 'combo.emptyText'),
                                    myStore: storeName
                                });
                            } 
                            // grids on demand by config
                            var grids = routeCfg['grids'] || [];
                            grids.forEach(function(gridCfg) {

                                var widgetName = gridCfg.widget || (name + 'list');
                                var listName  = config.name + '.view.' + name + '.' + widgetName;
                                if (gridCfg.preload) {
                                    Ext.onReady(function () { store.load() });
                                }
                                Ext.define(listName, {
                                    extend: 'Ext.grid.Panel',
                                    alias: 'widget.' + widgetName,
                                    multiSelect: true,
                                    title: translate(widgetName + ".title"),
                                    store: store,
                                    allowDeselect : true,
                                    requires: ['Ext.toolbar.Paging', 'Ext.ux.grid.FiltersFeature'],
                                    viewConfig: {
                                        listeners: {
                                            render: function(view) {
                                                var tooltip = gridCfg.tooltip || idTooltip;
                                                source.Utils.createToolTip(view, tooltip);
                                            }
                                        }
                                    },
                                    initComponent: function() {
                                        this.columns = _.filter(_.map(h.outputs, function(o) {

                                                                    var fields = gridCfg.fields,
                                                                        fieldCfg = gridCfg[o.name] || {};

                                                                    if (fields && fields.indexOf(o.name) == -1)
                                                                        return undefined;
                                                                    if (fieldCfg.hidden) {
                                                                        return undefined;
                                                                    } else {
                                                                        var defaultFlex = 10;
                                                                        if (o.name == 'id')
                                                                            defaultFlex = 1;
                                                                        return {
                                                                            "header" : translate(widgetName + "." + o.name + ".header", o.name),
                                                                            "dataIndex" : o.name,
                                                                            "flex" : fieldCfg.flex || defaultFlex,
                                                                            "filterable" : fieldCfg.filterable || true
                                                                            
                                                                        };
                                                                    }
                                                                }, function (c) { 
                                                                    return c != undefined; 
                                                                }));
                                        this.bbar = Ext.create('Ext.PagingToolbar', {
                                            store: store,
                                            displayInfo: true,
                                            displayMsg: translate(widgetName + '.paging.displayMsg'),
                                            emptyMsg: translate(listName + '.paging.emptyMsg'),
                                            items: ['-']
                                        });
                                        this.callParent(arguments);
                                    },
                                    features: [{ ftype : 'filters', encode:true}],
                                    dockedItems: (function () { 
                                        var toolbar = _.map(gridCfg.toolbar || [], function(tb) {
                                                if (endsWith(tb.xtype, 'combo')) {
                                                    return {
                                                        xtype: tb.xtype,
                                                        listeners: {
                                                            select: function(combo) {
                                                                store.filters.removeAtKey(tb.filterField);
                                                                store.addFilter(new Ext.util.Filter({
                                                                        id: tb.filterField,
                                                                        property: tb.filterField,
                                                                        value: ''+combo.getValue()
                                                                    }));
                                                            },
                                                            change: function(combo) {
                                                                if (combo.getValue() == '') {
                                                                    store.filters.removeAtKey(tb.filterField);
                                                                    store.reload();
                                                                }
                                                            }
                                                        }
                                                    };
                                                } else
                                                    console.log("unsupported toolbar item xtype: " + tb.xtype);
                                            });
                                        if (gridCfg.searchField != false) {
                                            toolbar.push({ 
                                                xtype: 'textfield',
                                                itemId: 'textSearch', 
                                                flex:1,
                                                emptyText: translate(widgetName + '.search'),
                                                listeners: {
                                                    change: {
                                                        buffer: 500,
                                                        fn: function(textField) {
                                                            store.filters.removeAtKey('query');
                                                            if (textField.getValue() != '') {
                                                                store.addFilter(new Ext.util.Filter({
                                                                    id: 'query',
                                                                    property: 'query',
                                                                    value: '' + textField.getValue()
                                                                }));
                                                            } else {
                                                                store.reload();
                                                            }
                                                        }
                                                    }
                                                },
                                                plugins: config.defaultTextFieldPlugins || []
                                            });
                                        }
                                        return toolbar;
                                    })()
                                });
                            });
                        });
                    });
                    
                     
                } else if (h.type != "GET") {
                   // TODO 

                }
                
            });
        });
    });
});
