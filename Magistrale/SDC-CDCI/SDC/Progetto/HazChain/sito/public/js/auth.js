// --- Auth UI: Login, Register, Session Management ---

// Gestione sessione
const token = localStorage.getItem('token');
const userStr = localStorage.getItem('user');

// Se non c'è token, reindirizza a login.html
if (!token || !userStr) {
    if (!window.location.pathname.endsWith('login.html')) {
        window.location.href = 'login.html';
    }
}

// Utente corrente
let currentUser = null;
if (userStr) {
    currentUser = JSON.parse(userStr);

    // Aggiornamento UI navbar
    window.addEventListener('DOMContentLoaded', () => {
        const authDiv = document.querySelector('.nav-auth');
        if (authDiv) {
            authDiv.innerHTML = `<button id="btn-logout" class="btn text" style="color: white;"> Logout (${currentUser.username})</button>`;
            document.getElementById('btn-logout').addEventListener('click', () => {
                localStorage.removeItem('token');
                localStorage.removeItem('user');
                window.location.href = 'login.html';
            });
        }
    });
}

// Redirect se già loggato e su login.html
if (window.location.pathname.endsWith('login.html')) {
    const _loginToken = localStorage.getItem('token');
    const _loginUser = localStorage.getItem('user');
    if (_loginToken && _loginUser) {
        const _u = JSON.parse(_loginUser);
        const _rolePages = {
            'LAB': 'lab.html',
            'TRANSPORT_LIGHT': 'van.html',
            'HUB': 'hub.html',
            'TRANSPORT_HAZ': 'truck.html',
            'LANDFILL': 'landfill.html',
            'SUPERUSER': 'alerts.html'
        };
        window.location.href = _rolePages[_u.role] || 'alerts.html';
    }
}

// --- Login Page Functions ---

function switchTab(tab) {
    document.getElementById('tab-login').classList.remove('active');
    document.getElementById('tab-register').classList.remove('active');
    document.getElementById('form-login').classList.remove('active');
    document.getElementById('form-register').classList.remove('active');

    document.getElementById(`tab-${tab}`).classList.add('active');
    document.getElementById(`form-${tab}`).classList.add('active');

    clearMessages();
}

function clearMessages() {
    const errEl = document.getElementById('error-msg');
    const sucEl = document.getElementById('success-msg');
    if (errEl) errEl.style.display = 'none';
    if (sucEl) sucEl.style.display = 'none';
}

function showError(msg) {
    const el = document.getElementById('error-msg');
    if (!el) return;
    el.textContent = msg;
    el.style.display = 'block';
    const sucEl = document.getElementById('success-msg');
    if (sucEl) sucEl.style.display = 'none';
}

function showSuccess(msg) {
    const el = document.getElementById('success-msg');
    if (!el) return;
    el.textContent = msg;
    el.style.display = 'block';
    const errEl = document.getElementById('error-msg');
    if (errEl) errEl.style.display = 'none';
}

// Login handler
async function handleLogin(e) {
    e.preventDefault();
    clearMessages();
    const btn = e.target.querySelector('button[type="submit"]');
    const originalText = btn.textContent;
    btn.textContent = 'Authenticating...';
    btn.disabled = true;

    const username = document.getElementById('login-username').value;
    const password = document.getElementById('login-password').value;

    try {
        const data = await API.login(username, password);

        localStorage.setItem('token', data.token);
        localStorage.setItem('user', JSON.stringify(data.user));

        const rolePages = {
            'LAB': 'lab.html',
            'TRANSPORT_LIGHT': 'van.html',
            'HUB': 'hub.html',
            'TRANSPORT_HAZ': 'truck.html',
            'LANDFILL': 'landfill.html',
            'SUPERUSER': 'alerts.html'
        };
        const targetPage = rolePages[data.user.role] || 'alerts.html';

        btn.textContent = 'Access Granted. Redirecting...';
        btn.style.background = 'var(--primary)';

        setTimeout(() => {
            window.location.href = targetPage;
        }, 500);
    } catch (err) {
        showError(err.message || 'Login failed');
        btn.textContent = originalText;
        btn.disabled = false;
    }
}

// Register handler
async function handleRegister(e) {
    e.preventDefault();
    clearMessages();
    const btn = e.target.querySelector('button[type="submit"]');
    const originalText = btn.textContent;
    btn.textContent = 'Registering...';
    btn.disabled = true;

    const username = document.getElementById('reg-username').value;
    const password = document.getElementById('reg-password').value;
    const role = document.getElementById('reg-role').value;

    try {
        await API.register(username, password, role);

        showSuccess('Registration successful! You can now access the network.');
        document.getElementById('form-register').reset();
        btn.textContent = 'Success!';

        setTimeout(() => {
            switchTab('login');
            document.getElementById('login-username').value = username;
            document.getElementById('login-password').focus();
            btn.textContent = originalText;
            btn.disabled = false;
        }, 1000);
    } catch (err) {
        showError(err.message || 'Registration failed');
        btn.textContent = originalText;
        btn.disabled = false;
    }
}
