#include <fstream>
#include <vector>
#include <thread>
#include <chrono>
#include <random>
using namespace std;
ifstream cin("input.txt");
ofstream cerr("output.txt");
ofstream cout("output.txt");
double eps = 1e-6;
template<typename R>
struct value_location{
    R val;
    int col_len;
    int row_len;
    value_location(R val,int id1,int id2):col_len(id1),row_len(id2),val(val){}
};
template<class T>
class matrix{
public:vector<vector<T>>Matrix;
    void string_swap(const int &id1,const int &id2){
        if(id1 == id2){return;}
        for(int i = 0;i < Matrix.size();i ++){
            swap(Matrix[id1][i],Matrix[id2][i]);
        }
    }
    void string_multiple(const int &id1,const double &value){
        if(abs(value - 1)< eps){return;}
        for(int i = 0;i < Matrix[0].size();i ++){
            Matrix[id1][i]*=value;
        }
    }
    void sum_string_by_string_multiplied_string(const int &id1,const int &id2,const double &value){
        if(abs(value) < eps){return;}
        for(int i = 0;i < Matrix.size();i ++){
            Matrix[id1][i] += Matrix[id2][i]*value;
        }
    }
    void multi(const matrix &a,const matrix &b,matrix &c,int l,int r){
        for(int i = l;i < r;i ++){
            for(int j = 0;j < c.Matrix[0].size();j ++){
                int sum = 0;
                for(int k = 0;k < a.Matrix[0].size();k ++){
                    sum += a.Matrix[i][k] * b.Matrix[k][j];
                }
                c.Matrix[i][j] = sum;
            }
        }
    }
    void big_num_multi(matrix &a,const double &val,int l,int r){
        for(int i = l;i < r;i ++){
            a.string_multiple(i,val);
        }
    }
    void big_two_mat_sum(const matrix &a,const matrix &b,matrix &c,int l,int r){
        for(int i = l;i < r;i ++){
            for(int j = 0;j < c.Matrix[0].size();j ++){
                c.Matrix[i][j] = a.Matrix[i][j] + b.Matrix[i][j];
            }
        }
    }
    void big_two_mat_vich(const matrix &a,const matrix &b,matrix &c,int l,int r){
        for(int i = l;i < r;i ++){
            for(int j = 0;j < c.Matrix[0].size();j ++){
                c.Matrix[i][j] = a.Matrix[i][j] - b.Matrix[i][j];
            }
        }
    }
    vector<pair<int,int>> divorce(int n,int k){
        vector<pair<int,int>>a;
        int det = max(n / k,1);
        a.reserve((n - 1 + det) / det);
for(int i = 0;i < (n - 1 + det) / det;i ++){
             a.emplace_back(i * det ,min((i + 1) * det,n));
        }
        return a;
    }
    void step1(matrix<T> &copy,matrix<T> &reversed_matrix,int l,int r){
        int Rows_count = copy.Matrix.size();
        for(int i = l;i < r;i ++){
            for(int j = i;j < Rows_count;j ++){
                if(abs(copy.Matrix[j][i]) >= eps){
                    reversed_matrix.string_swap(i,j);
                    reversed_matrix.string_multiple(i,1./copy.Matrix[i][i]);
                    copy.string_swap(i,j);
                    copy.string_multiple(i,1./copy.Matrix[i][i]);
                    break;
                }
            }
            if(abs(copy.Matrix[i][i]) < eps){
                cerr << "Determinat equal to zero";
                exit(-1);
            }
            for(int j = i + 1;j < reversed_matrix.Matrix.size();j ++){
                reversed_matrix.sum_string_by_string_multiplied_string(j,i,-copy.Matrix[j][i]/copy.Matrix[i][i]);
                copy.sum_string_by_string_multiplied_string(j,i,-copy.Matrix[j][i]/copy.Matrix[i][i]);
            }
        }
    }
    void step2(matrix<T> &copy,matrix<T> &reversed_matrix,int l,int r){
        for(int i = r - 1;i >=l;i --) {
            for (int j = i - 1; j >= 0; j--) {
                reversed_matrix.sum_string_by_string_multiplied_string(j,i,-copy.Matrix[j][i]);
                copy.sum_string_by_string_multiplied_string(j,i,-copy.Matrix[j][i]);
            }
        }
    }
public:
    matrix(long long n){
        if(n == 0) {
            cerr << "stupid matrix";
            exit(-1);
        }
        Matrix = vector<vector<T>>(n,vector<T>(n));
    }
    matrix(initializer_list<value_location<T>> a){
            int row_m = -1;
            int col_m = -1;
            for(auto it = a.begin();it != a.end();it ++){
                row_m = max(it->row_len,row_m);
                col_m = max(it->col_len,col_m);
            }
            if(row_m < 0 || col_m < 0){
                cerr << "Uncorect data";
                exit(-1);
            }
            Matrix = vector<vector<T>>(col_m + 1,vector<T>(row_m + 1));
            for(auto it = a.begin();it != a.end();it ++){
                Matrix[it->col_len][it->row_len] = it->val;
            }
    }
    matrix(matrix && source){
        this->Matrix = move(source.Matrix);//конструктор перемещений
    }
    matrix<T> &operator=(matrix<T> && other) noexcept {//конструктор перемещающего равенства
        this->Matrix = move(other.Matrix);
        return *this;
    }
    matrix(long long n,long long m){
        if(n * m == 0){
            cerr << "stupid matrix";
            exit(-1);
        }
        Matrix = vector<vector<T>>(n,vector<T>(m));
    }
    matrix(long  long n,long m,int x){
        if(this->Matrix.size() != this->Matrix[0].size() && x == 1){
            cerr<<"not square";
            exit(0);
        }
        vector<vector<T>>(n,vector<T>(m,0));
        if(x == 1){
            for(int i = 0;i < this->Matrix.size();i ++){
                this->Matrix[i][i] = 1;
            }
        }
    }
    matrix(const matrix &other){
        this->Matrix.resize(other.Matrix.size());
        for(int i = 0;i < this->Matrix.size();i ++){
            this->Matrix[i].resize(other.Matrix[i].size());
            for(int j = 0;j < this->Matrix[i].size();j ++){
                this->Matrix[i][j] = other.Matrix[i][j];
            }
        }
    }
    ~matrix()= default;
    void print_matrix(){
        cout << Matrix.size() << " " << Matrix[0].size() << endl;
        for(int i = 0;i < Matrix.size();i ++){
            for(int j = 0;j < Matrix[i].size();j ++){
                cout << Matrix[i][j] << " ";
            }
            cout << endl;
        }
    }
    void drunk_snake(){
        if(Matrix.empty() || Matrix.size() != Matrix[0].size()){
            cerr << "Incorrect matrix size";
            exit(-1);
        }
        if(typeid(T) == typeid(string) || typeid(T) == typeid(char)){
            cerr << "Incorrect data type";
            exit(-1);
        }
        int count = 0;
        int n = Matrix.size();
        int x = 0;
        int y = n - 1;
        for(int i = n - 1;i >= 0;i --){
            if(y == n - 1){
                for(int j = 0;j < i;j ++) {
                    Matrix[x][y] = n * n - count;
                    count++;
                    y--;
                }
                for(;x < n - 1;x ++) {
                    Matrix[x][y] = n * n - count;
                    count++;
                }
                Matrix[x][y] = n * n - count;
                count++;
                y ++;
            } else{
                for(int j = 0;j < i;j ++){
                    Matrix[x][y] = n * n - count;
                    count++;
                    x --;
                }
                for(;y < n - 1;y ++){
                    Matrix[x][y] = n * n - count;
                    count++;
                }
                Matrix[x][y] = n * n - count;
                count++;
                x ++;
            }
        }
    }
    void Z_number(){
        if(typeid(char) == typeid(T) || typeid(string) == typeid(T)){
            cerr << "char maybe be so small";
            exit(-1);
        }
        for(int i = 0;i < Matrix.size();i ++){
            for(int j = 0;j < Matrix[i].size();j ++){
                long long x = i*(Matrix[0].size()) + j + 1;
                    Matrix[i][j] =x;
            }
        }
    }
    matrix operator *(matrix const &other)/*перезагрузка * */{
        if(this->Matrix[0].size() != other.Matrix.size()){
            cerr << "Uncorrect matrix size";
            exit(-1);
        }
        if(typeid(this->Matrix[0][0]) != typeid(other.Matrix[0][0])){
            cerr << "Unequal matrix type";
            exit(-1);
        }
        matrix<T>ans(this->Matrix.size(),other.Matrix[0].size());
        int k = 8;
        int y = this->Matrix.size();
        ans.Matrix.resize(this->Matrix.size());
        vector<thread>thr;
        int del = y / k;
        if(del == 0){
            for(int i = 0;i < y;i ++){
                thr.emplace_back(&matrix::multi,this,cref(*this),cref(other),ref(ans),i,i + 1);
            }
            for(auto &th : thr){
                th.join();
            }
        }else {
            for (int i = 0; (i + 1) * del <= y; i++) {
                thr.emplace_back(&matrix::multi,this,cref(*this),cref(other),ref(ans),i * del,(i + 1) * del);

            }
            if (y % k != 0) {
                thr.emplace_back(&matrix::multi,this,cref(*this),cref(other),ref(ans),y - y % del,y);
            }
            for(auto &th : thr){
                th.join();
            }
        }
        return ans;
    }
    matrix operator=(const matrix<T> &a){
        this->Matrix.resize(a.Matrix.size());
        for(int i = 0;i < this->Matrix.size();i ++){
            this->Matrix[i].resize(a.Matrix[0].size());
            for(int j = 0;j < this->Matrix[0].size();j ++){
                this->Matrix[i][j] = a.Matrix[i][j];
            }
        }
        return *this;
    }
    matrix<double> operator*(const double &c){
        matrix<double>copy(this->Matrix.size(),this->Matrix[0].size());
        copy = *this;
        int k = 8;
        int n = this->Matrix.size();
        int del = max(n / k,1);
        vector<thread>th;
        th.reserve((n + del - 1) / del);
for(int i = 0;i < (n + del - 1) / del;i ++){
            th.push_back(thread(&matrix::big_num_multi, this,ref(copy),cref(c),i * del,min((i + 1) * del,n)));
        }
        for(auto &t:th){
            t.join();
        }
        return copy;
    }
    matrix operator+(const matrix<T> &a){
        if(this->Matrix.size() != a.Matrix.size() || this->Matrix[0].size() != a.Matrix[0].size()){
            cerr<<"Uncorrect data";
            exit(-1);
        }
        matrix<T>copy(a.Matrix.size(),a.Matrix[0].size());
        int k = 8;
        int n = this->Matrix.size();
        int del = max(n / k,1);
        vector<thread>th;
        th.reserve((n + del - 1) / del);
        for(int i = 0;i < (n + del - 1) / del;i ++){
            th.push_back(thread(&matrix::big_two_mat_sum, this,cref(*this),cref(a),ref(copy),i * del,min((i + 1) * del,n)));
        }
        for(auto &t:th){
            t.join();
        }
        return copy;
    }
    matrix operator-(const matrix<T> &a){
        if(this->Matrix.size() != a.Matrix.size() || this->Matrix[0].size() != a.Matrix[0].size()){
            cerr<<"Uncorrect data";
            exit(-1);
        }
        matrix<T>copy(a.Matrix.size(),a.Matrix[0].size());
        int k = 8;
        int n = this->Matrix.size();
        int del = max(n / k,1);
        vector<thread>th;
        th.reserve((n + del - 1) / del);
        for(int i = 0;i < (n + del - 1) / del;i ++){
            th.push_back(thread(&matrix::big_two_mat_vich, this,cref(*this),cref(a),ref(copy),i * del,min((i + 1) * del,n)));
        }
        for(auto &t:th){
            t.join();
        }
        return copy;
    }
    matrix<double> operator~(){
        if(typeid(T) != typeid(long long) && typeid(T) != typeid(int)  && typeid(T) != typeid(double) || this->Matrix.size() != this->Matrix[0].size()){
            cerr << "reversed matrix doesnt exist";
            exit(-1);
        }
        matrix<double>copy(this->Matrix.size());
        copy = *this;
        matrix<double>reversed_matrix(this->Matrix.size());
            for(int i = 0;i < this->Matrix.size();i ++) {
                for (int j = 0; j < this->Matrix.size(); j++) {
                    reversed_matrix.Matrix[i][j] = 1.;
                    if(i != j){
                        reversed_matrix.Matrix[i][j] --;
                    }
                }
            }
            int k =8;
            long long Rows_count = this->Matrix.size();
            vector<pair<int,int>>line = divorce(Rows_count,k);
            vector<thread>thr1;
            thr1.reserve(line.size());
            for(auto & i : line){
            thr1.push_back(thread(&matrix::step1,this,ref(copy),ref(reversed_matrix),i.first,i.second));
            }
            for(auto &th : thr1){
                th.join();
            }
        vector<thread>thr2;
        thr2.reserve(line.size());
        thr2.reserve(line.size());
        for(auto it = line.rbegin();it != line.rend();it += 1){
            thr2.push_back(thread(&matrix::step2,this,ref(copy),ref(reversed_matrix),it->first,it->second));
        }
        for(auto &th : thr2){
            th.join();
        }
            return reversed_matrix;
        }
};
template<class x,class y>
bool operator ==(matrix<x> a,matrix<y> b){
    if(typeid(x) != typeid(y) || a.Matrix.size() != b.Matrix.size() || a.Matrix[0].size() != b.Matrix[0].size()){
        return false;
    }
    for(int i = 0;i < a.Matrix.size();i ++){
        for(int j = 0;j < a.Matrix[0].size();j ++){
            if(a.Matrix[i][j] != b.Matrix[i][j]){
                return false;
            }
        }
    }
    return true;
}
template<class x>
bool operator ==(matrix<x> a,int y){
    if(y*(y - 1) != 0){
        return true;
    }
    for(int i = 0;i < a.Matrix.size();i ++){
        for(int j = 0;j < a.Matrix[0].size();j ++){
            if(i != j){
                if(a.Matrix[i][j] != 0){
                    return false;
                }
            }else{
                if(a.Matrix[i][j] != y){
                    return false;
                }
            }
        }
    }
    return true;
}
long long to_long_long(const string& s){
    long long x = 0;
    for(char i : s){
        if(i < '1'  || i > '9'){
            throw " IS NOT Z NUMBER";
        }
        x *= 10;
        x += i - '0';
    }
    return x;
}
string operator *(const string &a,const string &b){
    string c = a + b;
    return c;
}
int main() {
   matrix<double>a({value_location(-4.5,0,0),value_location(0.33,1,0)});
   a.print_matrix();
    return 0;
}
