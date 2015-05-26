// requires underscore.js
var yesodDsl = function(defs, strings, config) {
    function createStore(storeCls, storeId) {
        var store = Ext.create(storeCls, { storeId:storeId });
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
                    }
                    if (extraFilters) {
                        for (var i = 0; i < extraFilters.length; i++) {
                            c.store.addFilter(extraFilters[i], false);
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
        var parts = k.split(".");
        if (k in strings) {
            while (k in strings) {
                k = strings[k];
            }
            parts = k.split(".");

            return k;
        } else if (parts[parts.length - 1] in strings) {
            k = parts[parts.length - 1];
            while (k in strings) {
                k = strings[k];
            }
            return k;
            
        } else if (d) {
            return d;
        } else {
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
                        Ext.data.StoreManager.register(storeClass);


                        // ComboBox for entities with 'name' field
                        if (_.find(h.outputs, function (o) { return o.name == 'name'; }) || routeCfg.combo) {
                            var tpl = undefined, displayTpl = undefined, comboCfg = routeCfg.combo || {};

                            if (comboCfg.field) {
                                tpl = Ext.create('Ext.XTemplate',
                                    '<tpl for=".">',
                                        '<div class="x-boundlist-item">{' + comboCfg.field + '}</div>',
                                    '</tpl>');
                                displayTpl = Ext.create('Ext.XTemplate',
                                    '<tpl for=".">',
                                        '{' + comboCfg.field + '}',
                                    '</tpl>');
                            }
                            Ext.define(comboName, {
                                extend: config.name + '.view.Combo',
                                alias: 'widget.' + name + 'combo',
                                tpl: tpl, 
                                displayTpl: displayTpl,
                                emptyText: translate(name + 'combo.emptyText'),
                                myStore: storeName,
                                getFilters: comboCfg.getFilters
                            });
                        } 

                        var globalStore = createStore(storeClass, name);
                        // grids on demand by config
                        var grids = routeCfg['grids'] || [];
                        grids.forEach(function(gridCfg) {
                            var store = undefined;
                            if (gridCfg.globalStore == true) {
                                store = globalStore;
                            } else {
                                store = createStore(storeClass);
                            }


                            (gridCfg.filters || []).forEach(function (f) {
                                store.addFilter(new Ext.util.Filter({
                                        id: f.field,
                                        property: f.field,
                                        value: f.value,
                                    }), false)
                            });
                            var widgetName = gridCfg.widget || (name + 'list');
                            var listName  = config.name + '.view.' + name + '.' + widgetName;
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
                                    var grid = this;
                                    this.columns = _.map(gridCfg.columns, function(c) {
                                                        var field = undefined,
                                                            filterable = undefined,
                                                            renderer = undefined,
                                                            flex = undefined,
                                                            header = undefined;
                                                        if (typeof c == 'string') {
                                                            field = c;
                                                            header = c;
                                                        } else {
                                                            field = c.field;
                                                            filterable = c.filterable;
                                                            renderer = c.renderer;
                                                            flex = c.flex;
                                                            header = c.header || field;
                                                        }
                                                        return {
                                                            "header" : translate(widgetName + "." + header, header),
                                                            "dataIndex" : field,
                                                            "flex" : flex || 1,
                                                            "filterable" : filterable || true,
                                                            "renderer" : renderer
                                                        };
                                                    });
                                    var displayMsg = translate(widgetName + '.paging', true);
                                    if (displayMsg === true)
                                        displayMsg = translate(widgetName + '.title') + " {0} - {1} / {2}";
        
                                    this.bbar = Ext.create('Ext.PagingToolbar', {
                                        store: store,
                                        displayInfo: true,
                                        displayMsg: displayMsg,
                                        emptyMsg: translate(widgetName + '.emptyPaging'),
                                        items: ['-'].concat(_.map(gridCfg.bottomToolbar || [], function(tb) {
                                            return {
                                                name:tb.name,
                                                text:translate(widgetName +"." + tb.name),
                                                listeners: {
                                                    click: function(button) {
                                                        if (tb.action == 'remove') {
                                                            var selected = button.up(widgetName).getSelectionModel().getSelection();
                                                            Ext.MessageBox.confirm(translate(widgetName + '.' + tb.name + '.title'), 
                                                                                   translate(widgetName + '.' + tb.name + '.message').replace('{0}', ''+selected.length), 
                                                                                   function (btn) { 
                                                                                        if (btn == 'yes') { 
                                                                                            store.remove(selected);
                                                                                            store.sync()
                                                                                        }
                                                                                   });
                                                        }
                                                    }
                                                }
                                            };
                                        }))
                                    });
                                    this.callParent(arguments);

                                    if (gridCfg.preload != false) {
                                        store.load();
                                    }
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
};
