#include <iostream>
#include <vector>
#include <fstream>
using namespace std;

class fraction{
public:
    long long int x;
    long long int y=1;
    fraction(int a,int b){
        x=a;
        y=b;
    }
    fraction(int c){
        x=c;
        y=1;
    }
    fraction operator + (fraction f){
        long long int tempX=x*f.y+f.x*y;
        long long int tempY=f.y*y;
        return fraction(tempX,tempY);
    };
    fraction operator - (fraction f){
        long long int tempX=x*f.y-f.x*y;
        long long int tempY=f.y*y;
        return fraction(tempX,tempY);
    };
    fraction operator / (fraction f){
        return fraction(x*f.y,y*f.x);
    };
    fraction operator * (fraction f){
        if(f.x==0){
            return fraction(0);
        }
        return fraction(x*f.x,y*f.y);
    };
    bool operator > (fraction f){
      return (double)x/y>(double)f.x/f.y;
    };
    bool operator < (fraction f){
        return (double)x/y<(double)f.x/f.y;
    };
    bool operator == (double a){
        return (double)x/y==a;
    };
    bool operator != (double a){
        return !((double)x/y==a);
    };

};
void sortRows(vector<vector<fraction>>& matrix,vector<vector<fraction>>& imatrix,vector<fraction>& b, int select){
    for(int i=select+1;i<matrix.size();i++){
        for(int j=i-1;j>=select;j--){
            if(matrix[j+1][select]>matrix[j][select]){
                auto temp = matrix[j];
                matrix[j]=matrix[j+1];
                matrix[j+1]=temp;
                auto itemp = imatrix[j];
                imatrix[j]=imatrix[j+1];
                imatrix[j+1]=itemp;
                auto btemp = b[j];
                b[j]=b[j+1];
                b[j+1]=btemp;
            }
        }
    }
}
void rowOperations(vector<vector<fraction>>& matrix,vector<vector<fraction>>& imatrix,vector<fraction>& b,int startRow){
    for(int i=startRow+1;i<matrix.size();i++){
        fraction multi = matrix[i][startRow]/matrix[startRow][startRow];
        for(int j=0;j<matrix[i].size();j++){
            matrix[i][j]=matrix[i][j]-(matrix[startRow][j]*multi);
            imatrix[i][j]=imatrix[i][j]-(imatrix[startRow][j]*multi);
        }
        b[i]=b[i]-(b[startRow]*multi);
    }
}
vector<vector<fraction>> identityy(vector<vector<fraction>> matrix,vector<vector<fraction>> imatrix){
    for(int i=0;i<matrix.size();i++){
        for(int j=0;j<matrix.size();j++){
            if(i==j){
                fraction pivot=matrix[i][i];
                matrix[i][j]=matrix[i][j]/pivot;
                imatrix[i][j]=imatrix[i][j]/pivot;
            }
        }
    }
    fraction pivot = matrix[0][1];
    for(int i=0;i<3;i++){
        matrix[0][i]=matrix[0][i]-(matrix[1][i]*pivot);
        imatrix[0][i]=imatrix[0][i]-(imatrix[1][i]*pivot);
    }
    imatrix[0][2]=imatrix[0][2]-(imatrix[2][2]*matrix[0][2]);
    imatrix[1][2]=imatrix[1][2]-(imatrix[2][2]*matrix[1][2]);
    return imatrix;
}
vector<vector<fraction>> identity(int n){
    vector<vector<fraction>> iMatrix;
    for(int i=0;i<n;i++){
        vector<fraction> temp;
        for(int j=0;j<n;j++){
            if(i==j)
                temp.push_back(fraction(1));
            else
                temp.push_back(fraction(0));
        }
        iMatrix.push_back(temp);
    }
    return iMatrix;
}
void assign(vector<vector<fraction>>& matrix,vector<vector<fraction>>& imatrix,vector<fraction>& b,ifstream& infile){
    int size;
    infile>>size;
    for(int i=0;i<size;i++){
        vector<fraction> v;
        for(int j=0;j<size;j++){
            double temp;
            double bottom=1;
            infile>>temp;
            while((int)temp!=temp){
                temp*=10;
                bottom*=10;
            }
            v.push_back(fraction(temp,bottom));
        }
        double temp;
        double bottom=1;
        infile>>temp;
        while((int)temp!=temp){
            temp*=10;
            bottom*=10;
        }
        b.push_back(fraction(temp,bottom));
        matrix.push_back(v);
    }
    imatrix=identity(size);

}
void printMatrix(vector<vector<fraction>> matrix,ofstream& outfile){
    for(int i=0;i<matrix.size();i++){
        for(fraction j:matrix[i]){
            outfile<<j.x/(double)j.y<<" ";
        }
        outfile<<endl;
    }
}
vector<fraction> solve(vector<vector<fraction>> matrix,vector<fraction> b,int arbitrary){
    vector<fraction> reverseSolution;
    for(int i=0;i<arbitrary;i++){
        reverseSolution.push_back(fraction(0));
    }
    for(int i=matrix.size()-arbitrary-1;i>=0;i--){
        fraction sum(0);
        int count=0;
        if(reverseSolution.size()!=0)
        for(int j=matrix.size()-1;j>i;j--){
            sum=sum+(reverseSolution[count]*matrix[i][j]);
            count++;
        }
        b[i]=b[i]-sum;
        reverseSolution.push_back(b[i]/matrix[i][i]);
    }
    return reverseSolution;
}
void findRank(ifstream& infile,ofstream& outfile){
    vector<vector<fraction>> matrix;
    vector<vector<fraction>> iMatrix;
    vector<fraction> b;
    assign(matrix,iMatrix,b,infile);
    for(int i=0;i<matrix.size();i++){
        sortRows(matrix,iMatrix,b,i);
        if(matrix[i][i]==0){
            continue;
        }
        rowOperations(matrix,iMatrix,b,i);
    }
    int rank=matrix.size();
    for(int i=0;i<matrix.size();i++){
        if(matrix[i][i]==0)
            rank--;
    }
    // if 0 arbitrary if 1 no solution if 2 unique solution
    int selection;
    if(rank<matrix.size()){
        bool solvable=true;
        int lastColumn=matrix.size()-1;
        while(matrix[lastColumn][matrix.size()-1]==0){
            if(b[lastColumn]!=0){
                solvable= false;
                break;
            }
            lastColumn--;
        }
        if(solvable){
            selection=0;
        }
        else{
            selection=1;
        }
    }
    else{
        selection=2;
    }
    if(selection==0){
        int arbitrary=matrix.size()-rank;
        vector<fraction> sol;
        sol=solve(matrix,b,arbitrary);
        int count=1;
        outfile<<"Arbitrary variables:";
        for(int i=sol.size()-1;i>=0;i--){
            fraction h=sol[i];
            if(h.x==0){
                outfile<<"x"<<sol.size()-i<<" ";
            }
        }
        outfile<<endl;
        outfile<<"Arbitrary Solution:"<<endl;
        for(int i=sol.size()-1;i>=0;i--){
            fraction h=sol[i];
            outfile<<"x"<<count<<"="<<h.x/(double)h.y<<" ";
            count++;
        }
    }
    else if(selection==1){
        outfile<<"Inconsistent"<<" "<<"problem";
    }
    else{
        outfile<<"Unique Solution:";
        vector<fraction> sol;
        sol=solve(matrix,b,0);
        for(int i=sol.size()-1;i>=0;i--){
            fraction h=sol[i];
            outfile<<h.x/(double)h.y<<" ";
        }
        outfile<<endl<<"Inverted A:";
        iMatrix=identityy(matrix,iMatrix);
        printMatrix(matrix,outfile);
    }
}
int main() {
    ifstream infile1("../input1.txt");
    ofstream outfile1("../output1.txt");
    ifstream infile2("../input2.txt");
    ofstream outfile2("../output2.txt");
    ifstream infile3("../input3.txt");
    ofstream outfile3("../output3.txt");
    findRank(infile1,outfile1);
    findRank(infile2,outfile2);
    findRank(infile3,outfile3);

    return 0;
}