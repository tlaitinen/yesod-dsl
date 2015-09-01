// requires underscore.js
var yesodDsl = function(defs, __, config, onReady) {
    
    var preloadStores = [],
        cb = {
        onLogin : function() {
            preloadStores.forEach(function (s) { s.load(); } );
        }
    };
    var formItemXtypes = {
        integer: 'numberfield',
        number: 'numberfield',
        boolean: 'checkbox',
        string: 'textfield',
        day: 'datefield',
        timeofday: 'timefield'
    };
    function createToolTip(view, tipFunction) {
        view.tip = Ext.create('Ext.tip.ToolTip', {
            // The overall target element.
            target: view.el,
            minWidth: 300,
            // Each grid row causes its own seperate show and hide.
            delegate: view.itemSelector,
            // Moving within the row should not hide the tip.
            trackMouse: true,
            // Render immediately so that tip.body can be referenced prior to the first show.
            renderTo: Ext.getBody(),
            listeners: {
                // Change content dynamically depending on which element triggered the show.
                beforeshow: function (tip) {
                    var tooltip = tipFunction(view.getRecord(tip.triggerElement));
                    if(tooltip){
                        tip.update(tooltip);
                    } else {
                         tip.on('show', function(){
                             Ext.defer(tip.hide, 10, tip);
                         }, tip, {single: true});
                    }
                }
            }
        });
    }
    function createStore(storeCls, storeId) {
        var store = Ext.create(storeCls, { storeId:storeId });
        (config.defaultStoreFilters || []).forEach(function(cf) {
            store.addFilter(
                new Ext.util.Filter({
                    id: cf.field,
                    property: cf.field,
                    value: cf.value,
                    comparison: cf.comparison
                }), false
            );
        });
        return store;
    }
    function defineModel(modelName, fields, proxy, cb) {
        Ext.define(modelName, {
            extend: 'Ext.data.Model',
            fields: _.map(fields, function (f) {
                var name = f.name,
                    mapping = undefined;

                if (name == 'length') {
                    mapping = name;
                    name += '_';
                }
                var r = {
                    name : name,
                    type : (f.optional || f.references) ? "auto" : f.type
                };
                if (f.type == "utctime" || f.type == "day")
                    r.type = "date";
                if (f.type == "day") 
                    r.dateFormat = 'Y-m-d';
                    
                if ('default' in f)
                    r.defaultValue = f['default'];
                if (mapping != undefined) {
                    r.mapping = mapping;
                }
                return r;
            }),
            proxy: proxy
        }, cb);

    }
    function saveError(responseText) {
        Ext.Msg.alert(__('saveError.title'), 
                     __('saveError.message') + ": " + responseText);
    }

    function entityDefaults(entityName) {
        var r = {},
            e = _.find(defs.entities, function (e) { return e.name == entityName; });
        e.fields.forEach(function (f) {
            if (f.default) {
                r[f.name] = f.default;
            }
        });
        return r;
    }
    function gridWidgetName(routeName, gridCfg) {
        return gridCfg.widget || (routeName + 'list');
    }
    function gridClsName(routeName, gridCfg) {
        return config.name + '.view.' + routeName + '.' + gridWidgetName(routeName, gridCfg);
    }
    function formWidgetName(routeName, formCfg) {
        return formCfg.widget || (routeName + 'form');
    }
    function formClsName(routeName, formCfg) {
        return config.name + '.view.' + routeName + '.' + formWidgetName(routeName, formCfg);
    }

    function refreshGrids(routeName) {
        var reloaded = [];
        var routeCfg = config.routes[routeName] || {};
        (routeCfg.grids || []) .forEach(function (gridCfg) {
            Ext.ComponentQuery.query(gridWidgetName(routeName, gridCfg))
                .forEach(function (g) { 
                    var s = g.store;
                    if (!reloaded.some(function (rs) { return rs === s; })) {
                        s.reload();
                        reloaded.push(s);
                    }
                });
        });
    }
    function findGrid(widgetName) {
        var res = undefined;
        _.each(config.routes, function (r) {
            r.grids.forEach(function (g) {
                if (g.widget == widgetName)
                    res = g;
            });
        });
        return res;
    }
    function initFormFilters(ffs, form) {
        return _.map(ffs, function (fInfo) {
            if (typeof fInfo == 'string') {
                fInfo = {
                    name: fInfo,
                    field: 'id'
                };
            }
            var record = form.getForm().getRecord();

            var value = ('value' in fInfo) ? fInfo.value : record.get(fInfo.field)  ;
            var f = {
                id: fInfo.name,
                property: fInfo.name,
                value: (value == null) ? null : (''+ ((value != "") ? value : 0))
            };
            if (fInfo.op)
                f.comparison = fInfo.op;
            return new Ext.util.Filter(f);
        });
    }

    function initFormItem(h, formCfg, widgetName) {
        return function (i) {             
            var itemCfg = {};
            if (typeof i == 'string') {
                itemCfg.name = i;
            } else {
                itemCfg = i;
            }

            var field = _.find(h.outputs, function (o) { return o.name == itemCfg.name; }) || {};
            if (!('xtype' in itemCfg)) {
                if (field.references) {
                    var entityRoute = defaultRoute(field.references);
                    if (entityRoute) {
                        itemCfg.xtype = routeInfo(entityRoute).name + 'combo';
                    } else {
                        itemCfg.xtype = field.references + 'combo';
                    }
                } else {
                    itemCfg.xtype = formItemXtypes[field.type];
                }
            }
            var res = {
                xtype:itemCfg.xtype,
                name: itemCfg.name,
                allowBlank: itemCfg.allowBlank || true,
                labelWidth: itemCfg.labelWidth || formCfg.labelWidth || config.formLabelWidth || 120,
                autoScroll: itemCfg.height ? true : false,
            };
            if ('inputType' in itemCfg) {
                res.inputType = itemCfg.inputType;
            }
            if ('disabled' in itemCfg)
                res.disabled = itemCfg.disabled;
            if ('hidden' in itemCfg)
                res.hidden = itemCfg.hidden;
            if ('readOnly' in itemCfg)
                res.readOnly = itemCfg.readOnly;
            if ('extra' in itemCfg) {
                for (var prop in itemCfg.extra) {
                    if (itemCfg.extra.hasOwnProperty(prop)) {
                        res[prop] = itemCfg.extra[prop];
                    }
                }
            }


            if ('layout' in itemCfg)
                res.layout = itemCfg.layout;
            if ('minLength' in itemCfg) {
                res.minLength = itemCfg.minLength;
                if (itemCfg.minLengthText)
                    res.minLengthText = __(itemCfg.minLengthText);
            }

            if ('height' in itemCfg)
                res.height = itemCfg.height;
            var filters = (formCfg.filters || []).concat(itemCfg.filters || []);
            if (filters.length) {
                if (findGrid(itemCfg.xtype)) {
                    res.listeners = {
                        render: function(grid) {
                            initFormFilters(filters, grid.up('form')).forEach(function (f) {
                                grid.store.filters.removeAtKey(f.id);
                                grid.store.addFilter(f, false);
                            });
                            grid.store.reload();
                        },
                        idChanged: function(grid) {
                            initFormFilters(filters, grid.up('form')).forEach(function (f) {
                                grid.store.filters.removeAtKey(f.id);
                                grid.store.addFilter(f, false);
                            });
                            grid.store.reload();
                        }
                    };


                }
            }
            if (field.type == "boolean")
                res.inputValue = true;
            if (itemCfg.items) {
                res.items = _.map(itemCfg.items, function(i) { return initFormItem(h, formCfg, widgetName)(i); });
            }
            var text = __(widgetName + '.' + itemCfg.name);
            if (itemCfg.xtype == 'button') {
                res.text = text;
                res.listeners = {
                    click: function(button) {
                        if (itemCfg.form) {
                            var record = button.up('form').getForm().getRecord();
                            var form = itemCfg.form, formWidth = itemCfg.formWidth || config.subFormWidth || 510, formHeight = itemCfg.formHeight || config.subFormHeight || 530;
                            if (typeof form == "function")
                                form = form(record);
                            if (typeof formWidth == "function")
                                formWidth = formWidth(record);
                            if (typeof formHeight == "function")
                                formHeight = formHeight(record);


                            openFormWindow(form, formWidth, formHeight, record);
                        }
                    }
                };
            } else {
                res.fieldLabel = text;
            }
            return res;
        };
    }


    // customized ExtJS ComboBox to fetch a record by ID field (call configStore when value set)
    Ext.define(config.name + '.view.Combo',{
        extend: 'Ext.form.field.ComboBox',
        minChars : 1,
        typeAhead: true,
        store:'ext-empty-store',
        queryMode: 'remote',
        pageSize: config.comboPageSize || 10,
        valueField: 'id',
        displayField: 'name',
        plugins: config.defaultTextFieldPlugins || [],
        initComponent: function() {
            this.callParent(arguments);
            this.store = createStore(this.myStore);
            this.onBindStore(this.store);
        },
        configStore: function(extraFilters) {
            var v = this.getValue()
            this.store.addFilter(
                new Ext.util.Filter({
                    id: 'id',
                    property: 'id',
                    value: ''+v
                }), false
            );
            var c = this;
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
        }
    });
    function endsWith(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    }
    function idTooltip(record) { 
        return "ID " + record.get('id');
    }
    function defaultRoute(entityName) {
        return _.find(defs.routes, function (r) {
            return r.path.length == 1 && r.path[0].type == "string"
                && r.handlers.some(function (h) {
                    return h.type == 'GET'
                        && h.outputs.some(function (o) {
                            return o.name == 'id' && o.references == entityName;
                        });
                })
        });
    }
    function routeInfo(r) {
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
        return {
            name:name,
            url:url
        };
    }
    function openFormWindow(formName, formWidth, formHeight, record) {
        var winId = formName + record.get('id');
        var win = Ext.getCmp(winId);
        if (win) {
            win.toFront();
        } else {
            win = new Ext.Window({
                id: winId,
                width: formWidth, 
                height: formHeight,
                resizable:false,
                autoScroll:true,
                title: __(formName + ".title"),
                items: [ { xtype: formName } ]
            });
            win.down(formName).loadRecord(record);
            win.show();
            win.query('combobox').forEach(function (cb) { 
                if ('configStore' in cb) {
                    cb.configStore();
                }
            });

        }
    }
    defs.enums.forEach(function (e) {


        var storeName = config.name + '.store.' + e.name,
            comboName = config.name + '.view.' + e.name + '.Combo',
            widgetName = e.name + 'combo';
        Ext.define(storeName, {
                extend: 'Ext.data.Store',
                fields: [ 'text', 'value' ],
                data: {
                    values: _.map(e.values, function (v) {
                        return {
                            text: __(e.name + "." + v),
                            value: v
                        };
                    })
                },
                proxy: {
                    type: 'memory',
                    reader: {
                        type: 'json',
                        rootProperty: 'values'
                    }
                }
            }, function (storeCls) {
            Ext.data.StoreManager.register(storeCls);
            Ext.define(comboName, {
                extend: 'Ext.form.ComboBox',
                store: Ext.create(storeCls, { storeId:e.name }),
                alias: 'widget.' + widgetName,
                queryMode: 'local',
                displayField: 'text',
                valueField: 'value',
                emptyText: __(widgetName + '.emptyText')
            });
        });
    });
    defs.entities.forEach(function (e) {
        var modelName = config.name + '.model.' + e.name,
            route = defaultRoute(e.name);
        if (route) {
            var proxy = {
                type: 'rest',
                url: routeInfo(defaultRoute(e.name)).url,
                reader: {
                    type: 'json',
                    rootProperty: 'result',
                    totalProperty: 'totalCount'
                },
                listeners: {
                    exception: function (proxy, response, operation) {
                        if (response.request.options.method != 'GET')
                            saveError(response.responseText);
                    }
                }
            };
            defineModel(modelName, e.fields, proxy);

        }

    });
    var deferreds = {};

    defs.routes.forEach(function (r) {

        var info = routeInfo(r),
            name = info.name,
            url = info.url,
            proxyName = config.name + '.proxy.' + name,
            modelName = config.name + '.model.' + name,
            storeName = config.name + '.store.' + name,
            comboName = config.name + '.view.' + name + '.Combo';
        [proxyName, modelName, storeName, comboName].forEach(function (n) { 
            deferreds[n] = $.Deferred(); 
        });
        r.handlers.forEach(function (h) {
           
            var routeCfg = config.routes[name] || {};

            var grids = routeCfg.grids || [],
                forms = routeCfg.forms || [];
            grids.forEach(function(gridCfg) {
                deferreds[gridClsName(name, gridCfg)] = $.Deferred();
            });
            forms.forEach(function (formCfg) {
                deferreds[formClsName(name, formCfg)] = $.Deferred();
            });


            // create models and stores for GET handlers without parameters  
            if (h.type == "GET" && r.path.length == 1 && r.path[0].type == "string") {
Ext.define(proxyName, {
                        extend: 'Ext.data.proxy.Rest',
                        type: 'rest',
                        url: url,
                        reader: {
                            type: 'json',
                            rootProperty: 'result',
                            totalProperty: 'totalCount'
                        },
                        writer: {
                            writeAllFields: true
                        },
              
                        listeners: {
                            exception: function (proxy, response, operation) {
                                if (response.request.options.method != 'GET')
                                    saveError(response.responseText);
                            }
                        },
                        encodeFilters: function(filters) {
                            var min = [],
                                length = filters.length,
                                i = 0;


                            for (; i < length; i++) {
                                min[i] = filters[i].serialize();
                                if ('comparison' in filters[i])
                                    min[i].comparison = filters[i].comparison;
                            }

                            return this.applyEncoding(min);
                        }
                }, function(proxyCls) {
                    deferreds[proxyName].resolve();
                    var proxy = Ext.create(proxyCls);
                    defineModel(modelName, h.outputs, proxy, function(model) {
                        deferreds[modelName].resolve();
                        var storeDef = {
                            extend: 'Ext.data.Store',
                            filters: [],
                            model: modelName,
                            pageSize: config.defaultPageSize || 100,
                            remoteFilter: true,
                            remoteSort: true,
                            proxy: proxy,
                            autoSync : routeCfg.autoSync || false
                        };
                        Ext.define(storeName, storeDef, function(storeClass) {
                            deferreds[storeName].resolve();
                            Ext.data.StoreManager.register(storeClass);


                            // ComboBox for entities with 'name' field
                            if (_.find(h.outputs, function (o) { return o.name == 'name'; }) || routeCfg.combo) {
                                var tpl = undefined, displayTpl = undefined, comboCfg = routeCfg.combo || {};

                                var template = comboCfg.template || undefined;
                                if (comboCfg.field) {
                                    template = '{' + comboCfg.field + '}';
                                }
                                if (template != undefined) {
                                    tpl = Ext.create('Ext.XTemplate',
                                        '<tpl for=".">',
                                            '<div class="x-boundlist-item">{' + template + '}</div>',
                                        '</tpl>');
                                    displayTpl = Ext.create('Ext.XTemplate',
                                        '<tpl for=".">',
                                            '{' + template + '}',
                                        '</tpl>');
                                }

                                var cfg = {
                                    extend: config.name + '.view.Combo',
                                    alias: 'widget.' + name + 'combo',
                                    tpl: tpl, 
                                    displayTpl: displayTpl,
                                    emptyText: __(name + 'combo.emptyText'),
                                    myStore: storeName,
                                    getFilters: comboCfg.getFilters,
                                    forceSelection: comboCfg.forceSelection || false
                                };
                                    
                                Ext.define(comboName, cfg, function(comboCls) {
                                    deferreds[comboName].resolve();
                                });
                            } 

                            var globalStore = createStore(storeClass, name);


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
                                var widgetName = gridWidgetName(name, gridCfg);
                                var listName  = gridClsName(name, gridCfg);
                                Ext.define(listName, {
                                    extend: 'Ext.grid.Panel',
                                    alias: 'widget.' + widgetName,
                                    multiSelect: true,
                                    store: store,
                                    allowDeselect : true,
                                    title: __(widgetName + '.title'),
                                    requires: ['Ext.toolbar.Paging'],
                                    plugins: gridCfg.plugins || [],
                                    viewConfig: {
                                        listeners: {
                                            render: function(view) {

                                                if (gridCfg.tooltip)
                                                    createToolTip(view, gridCfg.tooltip);

                                                if (gridCfg.preload != false) {
                                                    store.load();
                                                }
                                            },
                                            celldblclick: function(grid, td, cellIndex, record, tr, rowIndex, e, eOpts) {
                                                if (gridCfg.form) {

                                                    var form = gridCfg.form, formWidth = gridCfg.formWidth || 610, formHeight = gridCfg.formHeight || 630;
                                                    if (typeof form == "function")
                                                        form = form(record);
                                                    if (typeof formWidth == "function")
                                                        formWidth = formWidth(record);
                                                    if (typeof formHeight == "function")
                                                        formHeight = formHeight(record);


                                                    openFormWindow(form, formWidth, formHeight, record);
                        
                                                }
                                            }
                                        }
                                    },
                                    initComponent: function() {
                                        var grid = this;
                                        this.columns = _.map(gridCfg.columns, function(c) {
                                                    if (typeof c == 'string') {
                                                        c = {
                                                            field:c,
                                                            header: c
                                                        };
                                                    } 
                                                    var header = c.header || c.field;
                                                    var r = {
                                                        header : __(widgetName + "." + header, header),
                                                        dataIndex : c.field,
                                                        flex : c.flex || 1
                                                    };
                                                    if (c.header == "")
                                                        r.header = "";


                                                    if ("renderer" in c)
                                                        r.renderer = c.renderer;
                                                    if ("editor" in c)
                                                        r.editor = c.editor;
                                                    
                                                    return r;
                                                });
                                        var displayMsg = __(widgetName + '.paging','x');
                                        if (displayMsg === 'x')
                                            displayMsg = __(widgetName + '.title') + " {0} - {1} / {2}";
            
                                        this.bbar = Ext.create('Ext.PagingToolbar', {
                                            store: store,
                                            displayInfo: true,
                                            displayMsg: displayMsg,
                                            emptyMsg: __(widgetName + '.emptyPaging'),
                                            items: ['-'].concat(_.map(gridCfg.bottomToolbar || [], function(tb) {
                                                return {
                                                    name:tb.name,
                                                    text:__(widgetName +"." + tb.name),
                                                    listeners: {
                                                        click: function(button) {
                                                            if (tb.action == 'remove') {
                                                                var selected = button.up(widgetName).getSelectionModel().getSelection();
                                                                store.remove(selected);
                                                                store.sync();
                                                            } else if (tb.action == 'new') {
                                                                var record = Ext.create(modelName, entityDefaults(entityName));
                                                                record.setId(0);

                                                                var form = gridCfg.form, formWidth = gridCfg.formWidth || 610, formHeight = gridCfg.formHeight || 630;
                                                                if (typeof form == "function")
                                                                    form = form(record);
                                                                if (typeof formWidth == "function")
                                                                    formWidth = formWidth(record);
                                                                if (typeof formHeight == "function")
                                                                    formHeight = formHeight(record);



                                                                openFormWindow(form, formWidth, formHeight, record);
                                                                                                                           } else if (tb.action == 'add') {
                                                                var record = Ext.create(modelName, entityDefaults(entityName));
                                                                record.setId(0);
                                                                record.save({
                                                                    success: function(rec, op) {
                                                                        var r = JSON.parse(op.getResponse().responseText)
                                                                        record.setId(r.id);
                                                                        store.insert(0, record);
                                                                        var rowEdit = grid.getPlugin('rowediting');
                                                                        if (rowEdit) {
                                                                            rowEdit.startEdit(0, 0);
                                                                        }



                                                                    }
                                                                });
                                                            }
                                                        }
                                                    }
                                                };
                                            }))
                                        });
                                        this.callParent(arguments);

                                    },
                                    dockedItems: (function () { 
                                        var toolbar = _.map(gridCfg.toolbar || [], function(tb) {
                                                if (endsWith(tb.xtype, 'combo')) {
                                                    return {
                                                        xtype: tb.xtype,
                                                        listeners: {
                                                            select: function(combo) {
                                                                store.filters.removeAtKey(tb.filterField);
                                                                var v = combo.getValue();
                                                                if (v != '__EMPTY_VALUE__') {
                                                                    store.addFilter(new Ext.util.Filter({
                                                                            id: tb.filterField,
                                                                            property: tb.filterField,
                                                                            value: ''+((v != undefined) ? v : 0)
                                                                        }));
                                                                }
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
                                                emptyText: __(widgetName + '.search'),
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
                                }, function(listCls) {
                                    deferreds[listName].resolve();
                                });
                            });

                            var entityName = (_.find(h.outputs, function (o) { return o.name == "id"; }) || {}).references,
                                entityRoute = defaultRoute(entityName),
                                entityRouteInfo = entityName ? routeInfo(entityRoute) : undefined;
                            forms.forEach(function (formCfg) {

                                var widgetName = formWidgetName(name, formCfg);
                                var formName  = formClsName(name, formCfg);
                                Ext.define(formName, {
                                    extend: 'Ext.form.Panel',
                                    alias: 'widget.' + widgetName,
                                    bodyPadding: formCfg.bodyPadding || 5,
                                    layout: {
                                        type: 'vbox',
                                        align: 'stretch'
                                    },
                                    buttons: _.map(formCfg.buttons || [ 'save', 'saveandclose', 'closewithoutsaving' ], 
                                                   function (n) {
                                                       var btn;
                                                       if (typeof n == 'string') {
                                                           btn = {
                                                               name: n,
                                                               action: n
                                                           };
                                                       } else {
                                                           btn = n;
                                                       }
                                                       return { 
                                                           text: __(widgetName + '.' + btn.name), 
                                                           name: btn.name,
                                                           listeners: {
                                                               click: function(button) {
                                                                   var win = button.up('window'),
                                                                       formWidget = button.up('form'),
                                                                       form = formWidget.getForm();
                                                                       valid = form.isValid(),
                                                                       validationMessage = 'validationError.message';
                                                                   if (formCfg.validation) {
                                                                       var msg = formCfg.validation(form.getValues());
                                                                       if (msg) {
                                                                           validationMessage = msg;
                                                                           valid = false;
                                                                       }
                                                                   }

                                                                   var canClose = btn.action == 'cancel' || valid;
                                                                    
                                                                   if (btn.action == 'save' || btn.action == 'saveandclose') {
                                                                       if (valid) {
                                                                           var store = Ext.getStore(entityRouteInfo.name),
                                                                               record = form.getRecord();
                                                                           if (btn.updateRecord != false) {
                                                                               form.updateRecord(record);
                                                                               h.outputs.forEach(function (o) {
                                                                                   if (o.references && record.get(o.name) == '') {
                                                                                       record.set(o.name, null);
                                                                                   }
                                                                               });
                                                                           }
                                                                           function formSubmit() {
                                                                               if (btn.url) {
                                                                                   Ext.Ajax.request({
                                                                                       url: btn.url.replace('(ID)',''+record.getId()),
                                                                                       params: form.getValues(),
                                                                                       failure: function(request) {
                                                                                           saveError(request.responseText);
                                                                                       }
                                                                                   });

                                                                               } else {
                                                                                   record.save({
                                                                                       success: function(rec, op) {
                                                                                           var r = JSON.parse(op.getResponse().responseText)
                                                                                           if (!record.getId()) {
                                                                                               record.setId(r.id);
                                                                                               formWidget.query('grid').forEach(function (g) { g.fireEvent('idChanged', g); });
                                                                                               refreshGrids(entityRouteInfo.name);
                                                                                           }
                                                                                           _.each(formCfg.success || [], function (cb) { cb(record); });
                                                                                       }
                                                                                   });
                                                                               }
                                                                           }
                                                                           $.when.apply($, _.map(formCfg.beforeSubmit || [], 
                                                                                                 function(bf) { return bf(form, record); } ))
                                                                                .then(formSubmit);


       
                                                                           


                                                                       } else {
                                                                           Ext.Msg.alert(__('validationError.title'),
                                                                                         __(validationMessage));
                                                                       }
                                                                   }
                                                                   if (canClose && (btn.action == 'saveandclose' || btn.action == 'closewithoutsaving' || btn.action == 'close')) {
                                                                       button.up('window').close();
                                                                   } 

                                                               }
                                                           }
                                                       };
                                                   }),
                                    items: _.flatten(_.map(formCfg.items, initFormItem(h, formCfg, widgetName))),

                                });

                            }, function (formCls) {
                                deferreds[formName].resolve();
                            });
                        });
                    });
                });
                
                 
            } else if (h.type != "GET") {
               // TODO 

            }
            
        });
    });
    var dfs = [];
    for (var df in deferreds) {
        dfs.push(df);
    }
    $.when.apply($, dfs).then(onReady);

    return {
        openFormWindow : openFormWindow,
        entityDefaults : entityDefaults
    }
};
